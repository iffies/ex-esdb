defmodule ExESDB.Cluster do
  @moduledoc false
  use GenServer

  require Logger

  alias ExESDB.LeaderWorker, as: LeaderWorker
  alias ExESDB.Options, as: Opts
  alias ExESDB.Themes, as: Themes

  defp ping?(node) do
    case :net_adm.ping(node) do
      :pong -> true
      _ -> false
    end
  end

  defp get_medal(leader, member),
    do: if(member == leader, do: "🏆", else: "🥈")

  defp join(store) do
    Opts.seed_nodes()
    |> Enum.map(fn seed ->
      if ping?(seed) do
        Logger.debug("#{Themes.cluster(node())} => Joining: #{inspect(seed)}")

        store
        |> :khepri_cluster.join(seed)
      end
    end)
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
    state[:store_id]
    |> join()

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

    Process.send_after(self(), :members, 2 * state[:timeout])
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

    {_, leader_node} =
      :ra_leaderboard.lookup_leader(store)

    new_state =
      state
      |> Keyword.put(:current_leader, leader_node)

    if node() == leader_node && current_leader != leader_node do
      IO.puts("⚠️⚠️ FOLLOW THE LEADER! ⚠️⚠️")

      store
      |> LeaderWorker.activate()
    end

    Process.send_after(self(), :check_leader, 2 * timeout)
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
    Process.send_after(self(), :join, timeout)
    Process.send_after(self(), :members, 2 * timeout)
    Process.send_after(self(), :check_leader, 5 * timeout)
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
