defmodule ExESDB.StoreCluster do
  @moduledoc false
  use GenServer

  require Logger

  alias ExESDB.LeaderWorker, as: LeaderWorker

  alias ExESDB.Options, as: Options

  # defp ping?(node) do
  #   case :net_adm.ping(node) do
  #     :pong => true
  #     _ => false
  #   end
  # end

  def leader?(store) do
    Logger.debug("checking if node is leader", component: :store_cluster, pid: node())

    case :ra_leaderboard.lookup_leader(store) do
      {_, leader_node} ->
        Logger.debug("node is leader: #{inspect(leader_node)}", component: :store_cluster, pid: node())
        node() == leader_node

      msg ->
        Logger.debug("leader lookup failed: #{inspect(msg)}", component: :store_cluster, pid: node())
        false
    end
  end

  def activate_leader_worker(store, leader_node) do
    if node() == leader_node do
      Logger.info("🚀 This node is the leader, activating LeaderWorker", component: :store_cluster, pid: self(), arrow: true)

      store |> LeaderWorker.activate()
    end
  end

  @doc """
  Registers a store with all Gater APIs in the cluster via Swarm.
  """
  def register_store(store, node \\ node()) do
    Logger.info("📝 Registering store #{inspect(store)} on node #{inspect(node)} with Gater APIs", component: :store_cluster, pid: self())

    # Create store config map that the gater API expects
    store_config = %{store_id: store}

    # Send registration message to all Gater APIs via ExESDBGater.API
    case broadcast_to_gater_apis({:register_store, store_config, node}) do
      :ok ->
        Logger.info("✅ Successfully registered store #{inspect(store)} on node #{inspect(node)}", component: :store_cluster, pid: self())

        :ok

      {:error, reason} ->
        Logger.error("❌ Failed to register store: #{inspect(reason)}", component: :store_cluster, pid: self())

        {:error, reason}
    end
  end

  @doc """
  Registers a store with Gater APIs, with automatic retry mechanism.

  This function handles the distributed store registry scenario where store nodes
  need to register themselves with gateway API services. Since libcluster is used
  for node discovery (not seed_nodes), this function provides resilience when:

  - No gateway API services are running yet (during startup)
  - Gateway API services are temporarily unavailable
  - Network partitions or other transient failures occur

  The retry mechanism uses exponential backoff with different delays for different
  error types:
  - :no_gater_apis -> 2s, 4s, 6s... up to 30s max
  - Other errors -> 1s, 2s, 3s... up to 10s max

  ## Parameters
  - store: The store atom identifier
  - node: The node name (defaults to current node)
  - attempt: Current attempt number (used for backoff calculation)

  ## Returns
  - :ok -> Registration successful
  - :retrying -> Registration failed, retry scheduled
  """
  def register_store_with_retry(store, node \\ node(), attempt \\ 1) do
    case register_store(store, node) do
      :ok ->
        :ok

      {:error, :no_gater_apis} ->
        # Exponential backoff, max 30s
        retry_delay = min(attempt * 2000, 30_000)

        Logger.info(
          "No Gater APIs available (attempt #{attempt}), retrying in #{retry_delay}ms",
          component: :store_cluster, pid: self()
        )

        Process.send_after(self(), {:retry_register_store, store, node, attempt + 1}, retry_delay)
        :retrying

      {:error, reason} ->
        # For other errors, retry with shorter delay
        # Exponential backoff, max 10s
        retry_delay = min(attempt * 1000, 10_000)

        Logger.warning(
          "Store registration failed (attempt #{attempt}): #{inspect(reason)}, retrying in #{retry_delay}ms",
          component: :store_cluster, pid: self()
        )

        Process.send_after(self(), {:retry_register_store, store, node, attempt + 1}, retry_delay)
        :retrying
    end
  end

  @doc """
  Unregisters a store from all Gater APIs in the cluster via Swarm.
  """
  def unregister_store(store, node \\ node()) do
    Logger.info(
      "Unregistering store #{inspect(store)} on node #{inspect(node)} from Gater APIs",
      component: :store_cluster, pid: self()
    )

    # Create store config map that the gater API expects
    store_config = %{store_id: store}

    # Send unregistration message to all Gater APIs via ExESDBGater.API
    case broadcast_to_gater_apis({:unregister_store, store_config, node}) do
      :ok ->
        Logger.info(
          "Successfully unregistered store #{inspect(store)} on node #{inspect(node)}",
          component: :store_cluster, pid: self()
        )

        :ok

      {:error, reason} ->
        Logger.warning(
          "Failed to unregister store: #{inspect(reason)}",
          component: :store_cluster, pid: self()
        )

        {:error, reason}
    end
  end

  defp get_medal(leader, member),
    do: if(member == leader, do: "🏆", else: "🥈")

  defp join_via_connected_nodes(store) do
    case Options.db_type() do
      :cluster ->
        # In cluster mode, use StoreCoordinator if available
        case Process.whereis(ExESDB.StoreCoordinator) do
          nil ->
            Logger.warning(
              "StoreCoordinator not available, trying direct join",
              component: :store_cluster, pid: node()
            )

            join_cluster_direct(store)

          _pid ->
            ExESDB.StoreCoordinator.join_cluster(store)
        end

      :single ->
        # In single mode, just ensure the store is started locally
        Logger.info("Running in single-node mode", component: :store_cluster, pid: node())
        :coordinator

      _ ->
        Logger.warning(
          "Unknown db_type, defaulting to single-node mode",
          component: :store_cluster, pid: node()
        )

        :coordinator
    end
  end

  defp join_cluster_direct(store) do
    # Fallback direct cluster join logic for when StoreCoordinator is not available
    connected_nodes = Node.list()

    if Enum.empty?(connected_nodes) do
      Logger.info(
        "No connected nodes, starting as single node cluster",
        component: :store_cluster, pid: node()
      )

      :no_nodes
    else
      Logger.info(
        "Attempting direct join to cluster via: #{inspect(connected_nodes)}",
        component: :store_cluster, pid: node()
      )

      # Try to find a node with an existing cluster
      case find_cluster_node(store, connected_nodes) do
        nil ->
          Logger.info(
            "No existing cluster found, starting as coordinator",
            component: :store_cluster, pid: node()
          )

          :coordinator

        target_node ->
          case :khepri_cluster.join(store, target_node) do
            :ok ->
              Logger.info(
                "Successfully joined cluster via #{inspect(target_node)}",
                component: :store_cluster, pid: node()
              )

              :ok

            {:error, reason} ->
              Logger.warning(
                "Failed to join via #{inspect(target_node)}: #{inspect(reason)}",
                component: :store_cluster, pid: node()
              )

              :failed
          end
      end
    end
  end

  defp report_leadership_change(old_leader, new_leader, _store) do
    Logger.info("🔄 LEADERSHIP CHANGED", component: :store_cluster, pid: self(), arrow: true)
    Logger.info("🔴 Previous leader: #{inspect(old_leader)}", component: :store_cluster, pid: self(), indent: 1)
    Logger.info("🟢 New leader:      🏆 #{inspect(new_leader)}", component: :store_cluster, pid: self(), indent: 1)

    # Check if we are the new leader
    if node() == new_leader do
      Logger.info("🚀 This node is now the leader!", component: :store_cluster, pid: self(), indent: 1)
    else
      Logger.info("📞 Following new leader: #{inspect(new_leader)}", component: :store_cluster, pid: self(), indent: 1)
    end

    # Also trigger a membership check to show updated leadership in membership
    Process.send(self(), :check_members, [])

    # Empty line removed - not needed with Logger
  end

  defp find_cluster_node(store, nodes) do
    Enum.find(nodes, fn node ->
      try do
        case :rpc.call(node, :khepri_cluster, :members, [store], 5000) do
          {:ok, members} when members != [] -> true
          _ -> false
        end
      rescue
        _ -> false
      catch
        _, _ -> false
      end
    end)
  end

  defp already_in_cluster?(store) do
    case :khepri_cluster.members(store) do
      {:ok, members} when length(members) > 1 -> true
      _ -> false
    end
  end

  defp should_handle_nodeup?(store) do
    case Options.db_type() do
      :cluster ->
        # In cluster mode, check if we should handle nodeup events
        case Process.whereis(ExESDB.StoreCoordinator) do
          nil ->
            # No coordinator available, check if we're already in a cluster
            not already_in_cluster?(store)

          _pid ->
            ExESDB.StoreCoordinator.should_handle_nodeup?(store)
        end

      :single ->
        # In single mode, never handle nodeup events for clustering
        false

      _ ->
        false
    end
  end

  defp leave(store) do
    case store |> :khepri_cluster.reset() do
      :ok ->
        Logger.info("Left cluster", component: :store_cluster, pid: node(), arrow: true)
        :ok

      {:error, reason} ->
        Logger.error(
          "Failed to leave cluster. reason: #{inspect(reason)}",
          component: :store_cluster, pid: node(), arrow: true
        )

        {:error, reason}
    end
  end

  defp members(store),
    do:
      store
      |> :khepri_cluster.members()

  defp broadcast_to_gater_apis(message) do
    try do
      # Get all registered Gater API PIDs using ExESDBGater.API
      gater_api_pids = ExESDBGater.API.get_gater_api_pids()

      if Enum.empty?(gater_api_pids) do
        Logger.warning("No Gater APIs found in Swarm registry", component: :store_cluster, pid: self())
        {:error, :no_gater_apis}
      else
        Logger.debug(
          "Broadcasting to #{length(gater_api_pids)} Gater APIs",
          component: :store_cluster, pid: self()
        )

        # Send message to all Gater API processes
        results =
          gater_api_pids
          |> Enum.map(fn pid ->
            try do
              GenServer.cast(pid, message)
              :ok
            rescue
              error ->
                Logger.warning(
                  "Failed to send message to Gater API #{inspect(pid)}: #{inspect(error)}",
                  component: :store_cluster, pid: self()
                )

                {:error, error}
            end
          end)

        # If any failed, return error; otherwise success
        case Enum.find(results, fn result -> match?({:error, _}, result) end) do
          nil -> :ok
          {:error, reason} -> {:error, {:partial_failure, reason}}
        end
      end
    rescue
      error ->
        Logger.error(
          "Error broadcasting to Gater APIs: #{inspect(error)}",
          component: :store_cluster, pid: self()
        )

        {:error, error}
    catch
      :exit, reason ->
        Logger.error(
          "Exit during Gater API broadcast: #{inspect(reason)}",
          component: :store_cluster, pid: self()
        )

        {:error, {:exit, reason}}
    end
  end

  @impl true
  def handle_info(:join, state) do
    store = state[:store_id]
    timeout = state[:timeout]

    case join_via_connected_nodes(store) do
      :ok ->
        Logger.info(
          "Successfully joined [#{inspect(store)}] cluster",
          component: :store_cluster, pid: node(), arrow: true
        )

        # Register store after successful cluster join
        register_store_with_retry(store)

      :coordinator ->
        Logger.info(
          "Acting as cluster coordinator, Khepri cluster already initialized",
          component: :store_cluster, pid: node(), arrow: true
        )

        # Register store when acting as coordinator
        register_store_with_retry(store)

      :no_nodes ->
        # Logger.warning(
        #   Themes.store_cluster(
        #     node(),
        #     " => No nodes discovered yet by LibCluster, will retry in #{timeout}ms"
        #   )
        # )
        #
        Process.send_after(self(), :join, timeout)

      :waiting ->
        Logger.alert(
          "Waiting for cluster coordinator, will retry in #{timeout * 2}ms",
          component: :store_cluster, pid: node(), arrow: true
        )

        Process.send_after(self(), :join, timeout * 2)

      :failed ->
        Logger.alert(
          "Failed to join discovered nodes, will retry in #{timeout * 3}ms",
          component: :store_cluster, pid: node(), arrow: true
        )

        Process.send_after(self(), :join, timeout * 3)
    end

    {:noreply, state}
  end

  @impl true
  def handle_info(:check_members, state) do
    leader = Keyword.get(state, :current_leader)
    store = state[:store_id]
    previous_members = Keyword.get(state, :previous_members, [])

    case store |> members() do
      {:error, reason} ->
        # Only log errors if they're different from the last error
        last_error = Keyword.get(state, :last_member_error)

        if last_error != reason do
          Logger.warning("⚠️⚠️ Failed to get store members. reason: #{inspect(reason)} ⚠️⚠️", component: :store_cluster, pid: self())
        end

        new_state = Keyword.put(state, :last_member_error, reason)
        Process.send_after(self(), :check_members, 5 * state[:timeout])
        {:noreply, new_state}

      {:ok, current_members} ->
        # Normalize members for comparison (sort by member name)
        normalized_current = Enum.sort_by(current_members, fn {_store, member} -> member end)
        normalized_previous = Enum.sort_by(previous_members, fn {_store, member} -> member end)

        # Only report if membership has changed
        if normalized_current != normalized_previous do
          Logger.info("MEMBERSHIP CHANGED", component: :store_cluster, pid: self(), arrow: true)

          # Report additions
          new_members = normalized_current -- normalized_previous

          if !Enum.empty?(new_members) do
            Enum.each(new_members, fn {_store, member} ->
              medal = get_medal(leader, member)
              Logger.info("✅ #{medal} #{inspect(member)} joined", component: :store_cluster, pid: self(), indent: 1)
            end)
          end

          # Report removals
          removed_members = normalized_previous -- normalized_current

          if !Enum.empty?(removed_members) do
            Enum.each(removed_members, fn {_store, member} ->
              Logger.info("❌ #{inspect(member)} left", component: :store_cluster, pid: self(), indent: 1)
            end)
          end

          # Show current full membership
          Logger.info("Current members:", component: :store_cluster, pid: self(), indent: 1)

          normalized_current
          |> Enum.each(fn {_store, member} ->
            medal = get_medal(leader, member)
            Logger.info("#{medal} #{inspect(member)}", component: :store_cluster, pid: self(), indent: 1)
          end)

          # Empty line removed - not needed with Logger
        end

        # Update state with current members and clear any previous error
        new_state =
          state
          |> Keyword.put(:previous_members, current_members)
          |> Keyword.delete(:last_member_error)

        Process.send_after(self(), :check_members, 5 * state[:timeout])
        {:noreply, new_state}
    end
  end

  @impl true
  def handle_info(:check_leader, state) do
    timeout = state[:timeout]
    previous_leader = Keyword.get(state, :current_leader)
    store = Keyword.get(state, :store_id)

    new_state =
      case :ra_leaderboard.lookup_leader(store) do
        {_, leader_node} ->
          cond do
            # Leadership changed to a different node
            previous_leader != nil && previous_leader != leader_node ->
              report_leadership_change(previous_leader, leader_node, store)

              # If we became the leader, activate LeaderWorker
              if node() == leader_node do
                store
                |> LeaderWorker.activate()
              end

              state
              |> Keyword.put(:current_leader, leader_node)

            # First time detecting leader or same leader
            previous_leader == nil ->
              if leader_node != nil do
                Logger.info("LEADER DETECTED: 🏆 #{inspect(leader_node)}", component: :store_cluster, pid: self(), arrow: true)

                activate_leader_worker(store, leader_node)
              end

              state |> Keyword.put(:current_leader, leader_node)

            # Same leader, no change needed
            true ->
              state
          end

        :undefined ->
          # Only report if we previously had a leader
          if previous_leader != nil do
            Logger.warning("⚠️ LEADERSHIP LOST: No leader found", component: :store_cluster, pid: self(), arrow: true)
          end

          state |> Keyword.put(:current_leader, nil)
      end

    Process.send_after(self(), :check_leader, timeout)
    {:noreply, new_state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, _pid, reason}, state) do
    state[:store_id]
    |> leave()

    msg = "🔻🔻 going down with reason: #{inspect(reason)} 🔻🔻"

    Logger.error(msg, component: :store_cluster, pid: self())
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, state) do
    Logger.error("exited with reason: #{inspect(reason)}", component: :store_cluster, pid: pid)

    state[:store_id]
    |> leave()

    {:noreply, state}
  end

  @impl true
  def handle_info({:nodeup, node}, state) do
    Logger.info("detected new node: #{inspect(node)}", component: :store_cluster, pid: self())

    store = state[:store_id]

    # Check if we should handle this nodeup event
    if should_handle_nodeup?(store) do
      Logger.info(
        "attempting coordinated cluster join due to new node",
        component: :store_cluster, pid: node()
      )

      case join_via_connected_nodes(store) do
        :ok ->
          Logger.info(
            "successfully joined cluster after nodeup event",
            component: :store_cluster, pid: node()
          )

          # Register store after successful join
          register_store_with_retry(store)
          # Trigger immediate membership and leadership checks after successful join
          Process.send(self(), :check_members, [])
          Process.send(self(), :check_leader, [])

        :coordinator ->
          Logger.info("acting as coordinator after nodeup event", component: :store_cluster, pid: node())
          # Register store when acting as coordinator
          register_store_with_retry(store)
          # Trigger immediate leadership check when acting as coordinator
          Process.send(self(), :check_leader, [])

        _ ->
          Logger.debug(
            "coordinated join not successful, will retry later",
            component: :store_cluster, pid: node()
          )
      end
    else
      Logger.debug("already in cluster, ignoring nodeup event", component: :store_cluster, pid: node())
      # Still trigger membership and leadership checks to detect any changes
      Process.send(self(), :check_members, [])
      Process.send(self(), :check_leader, [])
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:nodedown, node}, state) do
    Logger.info("detected node down: #{inspect(node)}", component: :store_cluster, pid: self())
    # Trigger immediate membership and leadership checks after node down event
    Process.send(self(), :check_members, [])
    Process.send(self(), :check_leader, [])
    {:noreply, state}
  end

  @impl true
  def handle_info({:retry_register_store, store, node, attempt}, state) do
    Logger.debug("Retrying store registration (attempt #{attempt})", component: :store_cluster, pid: self())
    register_store_with_retry(store, node, attempt)
    {:noreply, state}
  end

  @impl true
  def handle_info(_, state) do
    {:noreply, state}
  end

  ############# PLUMBING #############
  @impl true
  def terminate(reason, state) do
    Logger.warning("terminating with reason: #{inspect(reason)}", component: :store_cluster, pid: self())

    state[:store_id]
    |> leave()

    :ok
  end

  @impl true
  def init(config) do
    timeout = config[:timeout] || 1000
    state = Keyword.put(config, :timeout, timeout)
    Logger.info("is UP", component: :store_cluster, pid: self())
    Process.flag(:trap_exit, true)

    # Subscribe to LibCluster events
    :ok = :net_kernel.monitor_nodes(true)

    Process.send_after(self(), :join, timeout)
    Process.send_after(self(), :check_members, 10 * timeout)
    Process.send_after(self(), :check_leader, timeout)
    {:ok, state}
  end

  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  def child_spec(opts),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 10_000,
      type: :worker
    }
end
