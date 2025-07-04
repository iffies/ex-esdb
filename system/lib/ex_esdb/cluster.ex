defmodule ExESDB.Cluster do
  @moduledoc false
  use GenServer

  require Logger

  alias ExESDB.LeaderWorker, as: LeaderWorker
  alias ExESDB.Themes, as: Themes

  # defp ping?(node) do
  #   case :net_adm.ping(node) do
  #     :pong -> true
  #     _ -> false
  #   end
  # end

  def leader?(store) do
    {_, leader_node} =
      :ra_leaderboard.lookup_leader(store)

    node() == leader_node
  end

  defp get_medal(leader, member),
    do: if(member == leader, do: "🏆", else: "🥈")

  alias ExESDB.ClusterCoordinator

  defp join_via_connected_nodes(store) do
    ClusterCoordinator.join_cluster(store)
  end

  defp leave(store) do
    case store |> :khepri_cluster.reset() do
      :ok ->
        IO.puts("#{Themes.cluster(node())} => Left cluster")
        :ok

      {:error, reason} ->
        Logger.error(
          "#{Themes.cluster(node())} => Failed to leave cluster. reason: #{inspect(reason)}"
        )

        {:error, reason}
    end
  end

  defp members(store),
    do:
      store
      |> :khepri_cluster.members()

  @impl true
  def handle_info(:join, state) do
    store = state[:store_id]
    timeout = state[:timeout]

    case join_via_connected_nodes(store) do
      :ok ->
        Logger.info("#{Themes.cluster(node())} => Successfully joined Khepri cluster")

      :coordinator ->
        Logger.info(
          "#{Themes.cluster(node())} => Acting as cluster coordinator, Khepri cluster already initialized"
        )

      :no_nodes ->
        Logger.warning(
          "#{Themes.cluster(node())} => No nodes discovered yet by LibCluster, will retry in #{timeout}ms"
        )

        Process.send_after(self(), :join, timeout)

      :waiting ->
        Logger.alert(
          "#{Themes.cluster(node())} => Waiting for cluster coordinator, will retry in #{timeout * 2}ms"
        )

        Process.send_after(self(), :join, timeout * 2)

      :failed ->
        Logger.alert(
          "#{Themes.cluster(node())} => Failed to join discovered nodes, will retry in #{timeout * 3}ms"
        )

        Process.send_after(self(), :join, timeout * 3)
    end

    {:noreply, state}
  end

  @impl true
  def handle_info(:members, state) do
    IO.puts("\nMEMBERS")

    leader = Keyword.get(state, :current_leader)
    store = state[:store_id]

    case store
         |> members() do
      {:error, reason} ->
        IO.puts("⚠️⚠️ Failed to get store members. reason: #{inspect(reason)} ⚠️⚠️")

      {:ok, members} ->
        members
        |> Enum.each(fn {_store, member} ->
          medal = get_medal(leader, member)
          IO.puts("#{medal} #{inspect(member)}")
        end)
    end

    Process.send_after(self(), :members, 5 * state[:timeout])
    {:noreply, state}
  end

  @impl true
  def handle_info(:check_leader, state) do
    timeout = state[:timeout]

    current_leader =
      state
      |> Keyword.get(:current_leader)

    store =
      state
      |> Keyword.get(:store_id)

    new_state =
      case :ra_leaderboard.lookup_leader(store) do
        {_, leader_node} ->
          if node() == leader_node && current_leader != leader_node do
            IO.puts("⚠️⚠️ FOLLOW THE LEADER! ⚠️⚠️")

            store
            |> LeaderWorker.activate()
          end

          state
          |> Keyword.put(:current_leader, leader_node)

        :undefined ->
          IO.puts("⚠️⚠️ No leader found. ⚠️⚠️")
          state
      end

    Process.send_after(self(), :check_leader, timeout)
    {:noreply, new_state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    state[:store_id]
    |> leave()

    IO.puts("🔻🔻 #{Themes.cluster(pid)} going down with reason: #{inspect(reason)} 🔻🔻")
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, state) do
    IO.puts("#{Themes.cluster(pid)} exited with reason: #{inspect(reason)}")

    state[:store_id]
    |> leave()

    {:noreply, state}
  end

  @impl true
  def handle_info({:nodeup, node}, state) do
    Logger.info("#{Themes.cluster(self())} detected new node: #{inspect(node)}")

    store = state[:store_id]

    # Check if we should handle this nodeup event
    if ClusterCoordinator.should_handle_nodeup?(store) do
      Logger.info(
        "#{Themes.cluster(node())} attempting coordinated cluster join due to new node"
      )

      case join_via_connected_nodes(store) do
        :ok ->
          Logger.info(
            "#{Themes.cluster(node())} successfully joined cluster after nodeup event"
          )

        :coordinator ->
          Logger.info("#{Themes.cluster(node())} acting as coordinator after nodeup event")

        _ ->
          Logger.debug(
            "#{Themes.cluster(node())} coordinated join not successful, will retry later"
          )
      end
    else
      Logger.debug(
        "#{Themes.cluster(node())} already in cluster, ignoring nodeup event"
      )
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:nodedown, node}, state) do
    Logger.info("#{Themes.cluster(self())} detected node down: #{inspect(node)}")
    {:noreply, state}
  end

  @impl true
  def handle_info(_, state) do
    {:noreply, state}
  end

  ############# PLUMBING #############
  @impl true
  def terminate(reason, state) do
    Logger.warning("#{Themes.cluster(self())} terminating with reason: #{inspect(reason)}")

    state[:store_id]
    |> leave()

    :ok
  end

  @impl true
  def init(config) do
    timeout = config[:timeout] || 1000
    state = Keyword.put(config, :timeout, timeout)
    IO.puts("#{Themes.cluster(self())} is UP")
    Process.flag(:trap_exit, true)

    # Subscribe to LibCluster events
    :ok = :net_kernel.monitor_nodes(true)

    Process.send_after(self(), :join, timeout)
    Process.send_after(self(), :members, 10 * timeout)
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
