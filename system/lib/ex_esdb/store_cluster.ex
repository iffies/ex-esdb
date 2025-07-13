defmodule ExESDB.StoreCluster do
  @moduledoc false
  use GenServer

  require Logger

  alias ExESDB.LeaderWorker, as: LeaderWorker

  alias ExESDB.Options, as: Options
  alias ExESDB.Themes, as: Themes

  # defp ping?(node) do
  #   case :net_adm.ping(node) do
  #     :pong => true
  #     _ => false
  #   end
  # end

  def leader?(store) do
    Logger.debug(Themes.store_cluster(node(), "checking if node is leader"))

    case :ra_leaderboard.lookup_leader(store) do
      {_, leader_node} ->
        Logger.debug(Themes.store_cluster(node(), "node is leader: #{inspect(leader_node)}"))
        node() == leader_node

      msg ->
        Logger.debug(Themes.store_cluster(node(), "leader lookup failed: #{inspect(msg)}"))
        false
    end
  end

  def activate_leader_worker(store, leader_node) do
    if node() == leader_node do
      IO.puts(
        Themes.store_cluster(
          self(),
          "==> 🚀 This node is the leader, activating LeaderWorker"
        )
      )

      store |> LeaderWorker.activate()
    end
  end

  @doc """
  Registers a store with all Gater APIs in the cluster via Swarm.
  """
  def register_store(store, node \\ node()) do
    IO.puts(
      Themes.store_cluster(
        self(),
        "📝 Registering store #{inspect(store)} on node #{inspect(node)} with Gater APIs"
      )
    )

    # Create store config map that the gater API expects
    store_config = %{store_id: store}

    # Send registration message to all Gater APIs via ExESDBGater.API
    case broadcast_to_gater_apis({:register_store, store_config, node}) do
      :ok ->
        IO.puts(
          Themes.store_cluster(
            self(),
            "✅ Successfully registered store #{inspect(store)} on node #{inspect(node)}"
          )
        )

        :ok

      {:error, reason} ->
        IO.puts(Themes.store_cluster(self(), "❌ Failed to register store: #{inspect(reason)}"))

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
          Themes.store_cluster(
            self(),
            "No Gater APIs available (attempt #{attempt}), retrying in #{retry_delay}ms"
          )
        )

        Process.send_after(self(), {:retry_register_store, store, node, attempt + 1}, retry_delay)
        :retrying

      {:error, reason} ->
        # For other errors, retry with shorter delay
        # Exponential backoff, max 10s
        retry_delay = min(attempt * 1000, 10_000)

        Logger.warning(
          Themes.store_cluster(
            self(),
            "Store registration failed (attempt #{attempt}): #{inspect(reason)}, retrying in #{retry_delay}ms"
          )
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
      Themes.store_cluster(
        self(),
        "Unregistering store #{inspect(store)} on node #{inspect(node)} from Gater APIs"
      )
    )

    # Create store config map that the gater API expects
    store_config = %{store_id: store}

    # Send unregistration message to all Gater APIs via ExESDBGater.API
    case broadcast_to_gater_apis({:unregister_store, store_config, node}) do
      :ok ->
        Logger.info(
          Themes.store_cluster(
            self(),
            "Successfully unregistered store #{inspect(store)} on node #{inspect(node)}"
          )
        )

        :ok

      {:error, reason} ->
        Logger.warning(
          Themes.store_cluster(self(), "Failed to unregister store: #{inspect(reason)}")
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
              Themes.store_cluster(node(), "StoreCoordinator not available, trying direct join")
            )

            join_cluster_direct(store)

          _pid ->
            ExESDB.StoreCoordinator.join_cluster(store)
        end

      :single ->
        # In single mode, just ensure the store is started locally
        Logger.info(Themes.store_cluster(node(), "Running in single-node mode"))
        :coordinator

      _ ->
        Logger.warning(
          Themes.store_cluster(node(), "Unknown db_type, defaulting to single-node mode")
        )

        :coordinator
    end
  end

  defp join_cluster_direct(store) do
    # Fallback direct cluster join logic for when StoreCoordinator is not available
    connected_nodes = Node.list()

    if Enum.empty?(connected_nodes) do
      Logger.info(
        Themes.store_cluster(node(), "No connected nodes, starting as single node cluster")
      )

      :no_nodes
    else
      Logger.info(
        "#{Themes.store_cluster(node(), "Attempting direct join to cluster via: #{inspect(connected_nodes)}")}"
      )

      # Try to find a node with an existing cluster
      case find_cluster_node(store, connected_nodes) do
        nil ->
          Logger.info(
            Themes.store_cluster(node(), "No existing cluster found, starting as coordinator")
          )

          :coordinator

        target_node ->
          case :khepri_cluster.join(store, target_node) do
            :ok ->
              Logger.info(
                Themes.store_cluster(
                  node(),
                  "Successfully joined cluster via #{inspect(target_node)}"
                )
              )

              :ok

            {:error, reason} ->
              Logger.warning(
                Themes.store_cluster(
                  node(),
                  "Failed to join via #{inspect(target_node)}: #{inspect(reason)}"
                )
              )

              :failed
          end
      end
    end
  end

  defp report_leadership_change(old_leader, new_leader, _store) do
    IO.puts("\n#{Themes.store_cluster(self(), "🔄 LEADERSHIP CHANGED")}")
    IO.puts("  🔴 Previous leader: #{inspect(old_leader)}")
    IO.puts("  🟢 New leader:      🏆 #{inspect(new_leader)}")

    # Check if we are the new leader
    if node() == new_leader do
      IO.puts("  🚀 This node is now the leader!")
    else
      IO.puts("  📞 Following new leader: #{inspect(new_leader)}")
    end

    # Also trigger a membership check to show updated leadership in membership
    Process.send(self(), :check_members, [])

    IO.puts("")
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
        IO.puts(Themes.store_cluster(node(), "=> Left cluster"))
        :ok

      {:error, reason} ->
        Logger.error(
          Themes.store_cluster(node(), "=> Failed to leave cluster. reason: #{inspect(reason)}")
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
        Logger.warning(Themes.store_cluster(self(), "No Gater APIs found in Swarm registry"))
        {:error, :no_gater_apis}
      else
        Logger.debug(
          Themes.store_cluster(self(), "Broadcasting to #{length(gater_api_pids)} Gater APIs")
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
                  Themes.store_cluster(
                    self(),
                    "Failed to send message to Gater API #{inspect(pid)}: #{inspect(error)}"
                  )
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
          Themes.store_cluster(self(), "Error broadcasting to Gater APIs: #{inspect(error)}")
        )

        {:error, error}
    catch
      :exit, reason ->
        Logger.error(
          Themes.store_cluster(self(), "Exit during Gater API broadcast: #{inspect(reason)}")
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
          Themes.store_cluster(node(), "=> Successfully joined [#{inspect(store)}] cluster")
        )

        # Register store after successful cluster join
        register_store_with_retry(store)

      :coordinator ->
        Logger.info(
          Themes.store_cluster(
            node(),
            " => Acting as cluster coordinator, Khepri cluster already initialized"
          )
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
          Themes.store_cluster(
            node(),
            " => Waiting for cluster coordinator, will retry in #{timeout * 2}ms"
          )
        )

        Process.send_after(self(), :join, timeout * 2)

      :failed ->
        Logger.alert(
          Themes.store_cluster(
            node(),
            " => Failed to join discovered nodes, will retry in #{timeout * 3}ms"
          )
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
          IO.puts("⚠️⚠️ Failed to get store members. reason: #{inspect(reason)} ⚠️⚠️")
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
          IO.puts("\n#{Themes.store_cluster(self(), "MEMBERSHIP CHANGED")}")

          # Report additions
          new_members = normalized_current -- normalized_previous

          if !Enum.empty?(new_members) do
            Enum.each(new_members, fn {_store, member} ->
              medal = get_medal(leader, member)
              IO.puts("  ✅ #{medal} #{inspect(member)} joined")
            end)
          end

          # Report removals
          removed_members = normalized_previous -- normalized_current

          if !Enum.empty?(removed_members) do
            Enum.each(removed_members, fn {_store, member} ->
              IO.puts("  ❌ #{inspect(member)} left")
            end)
          end

          # Show current full membership
          IO.puts("\n  Current members:")

          normalized_current
          |> Enum.each(fn {_store, member} ->
            medal = get_medal(leader, member)
            IO.puts("  #{medal} #{inspect(member)}")
          end)

          IO.puts("")
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
                IO.puts(
                  Themes.store_cluster(self(), "==> LEADER DETECTED: 🏆 #{inspect(leader_node)}")
                )

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
            IO.puts(Themes.store_cluster(self(), "==> ⚠️ LEADERSHIP LOST: No leader found"))
          end

          state |> Keyword.put(:current_leader, nil)
      end

    Process.send_after(self(), :check_leader, timeout)
    {:noreply, new_state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    state[:store_id]
    |> leave()

    msg = "🔻🔻 #{Themes.store_cluster(pid, "going down with reason: #{inspect(reason)}")} 🔻🔻"

    IO.puts(msg)
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, state) do
    IO.puts("#{Themes.store_cluster(pid, "exited with reason: #{inspect(reason)}")}")

    state[:store_id]
    |> leave()

    {:noreply, state}
  end

  @impl true
  def handle_info({:nodeup, node}, state) do
    Logger.info("#{Themes.store_cluster(self(), "detected new node: #{inspect(node)}")}")

    store = state[:store_id]

    # Check if we should handle this nodeup event
    if should_handle_nodeup?(store) do
      Logger.info(
        Themes.store_cluster(node(), "attempting coordinated cluster join due to new node")
      )

      case join_via_connected_nodes(store) do
        :ok ->
          Logger.info(
            Themes.store_cluster(node(), "successfully joined cluster after nodeup event")
          )

          # Register store after successful join
          register_store_with_retry(store)
          # Trigger immediate membership and leadership checks after successful join
          Process.send(self(), :check_members, [])
          Process.send(self(), :check_leader, [])

        :coordinator ->
          Logger.info(Themes.store_cluster(node(), "acting as coordinator after nodeup event"))
          # Register store when acting as coordinator
          register_store_with_retry(store)
          # Trigger immediate leadership check when acting as coordinator
          Process.send(self(), :check_leader, [])

        _ ->
          Logger.debug(
            Themes.store_cluster(node(), "coordinated join not successful, will retry later")
          )
      end
    else
      Logger.debug(Themes.store_cluster(node(), "already in cluster, ignoring nodeup event"))
      # Still trigger membership and leadership checks to detect any changes
      Process.send(self(), :check_members, [])
      Process.send(self(), :check_leader, [])
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:nodedown, node}, state) do
    Logger.info(Themes.store_cluster(self(), "detected node down: #{inspect(node)}"))
    # Trigger immediate membership and leadership checks after node down event
    Process.send(self(), :check_members, [])
    Process.send(self(), :check_leader, [])
    {:noreply, state}
  end

  @impl true
  def handle_info({:retry_register_store, store, node, attempt}, state) do
    Logger.debug(Themes.store_cluster(self(), "Retrying store registration (attempt #{attempt})"))
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
    Logger.warning(Themes.store_cluster(self(), "terminating with reason: #{inspect(reason)}"))

    state[:store_id]
    |> leave()

    :ok
  end

  @impl true
  def init(config) do
    timeout = config[:timeout] || 1000
    state = Keyword.put(config, :timeout, timeout)
    IO.puts(Themes.store_cluster(self(), "is UP"))
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
