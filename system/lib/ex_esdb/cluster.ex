defmodule ExESDB.Cluster do
  @moduledoc false
  use GenServer

  require Logger
  require Colors

  alias ExESDB.Themes, as: Themes
  alias ExESDB.Options, as: Opts

  # defp ping?(node) do
  #   case :net_adm.ping(node) do
  #     :pong -> true
  #     _ -> false
  #   end
  # end

  defp join(store) do
      Opts.seed_nodes()
      |> Enum.map(
      fn seed ->
        Logger.debug("#{Themes.cluster(node())} => Joining: #{inspect(seed)}")
        store
        |> :khepri_cluster.join(seed)
      end)
  end

  defp leave(store) do
    case store |> :khepri_cluster.reset() do
     :ok ->
        Logger.warning("#{Themes.cluster(node())} => Left cluster")
        :ok
     {:error, reason} ->
        Logger.error("#{Themes.cluster(node())} => Failed to leave cluster. reason: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp members(store)  do
    case store
      |>:khepri_cluster.members() do
     {:error, reason} ->
        Logger.error("#{Themes.cluster(node())} => Failed to get store members. reason: #{inspect(reason)}")
      members ->
        Logger.debug("#{Themes.cluster(node())} => members: #{inspect(members, pretty: true)}")
    end
  end

  @impl true
  def handle_info(:join, state) do
    state[:store_id]
    |> join()
    {:noreply, state}
  end

  @impl true
  def handle_info(:members, state) do
    state[:store_id]
    |> members()
    Process.send_after(self(), :members, 2 * state[:timeout])
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, state) do
    Logger.warning("#{Themes.cluster(pid)} exited with reason: #{inspect(reason)}")
    state[:store_id]
    |> leave()
    {:noreply, state}
  end

  ############# PLUMBING #############
  @impl true
  def terminate(reason, state) do

    Logger.warning("#{Themes.cluster(self())} terminating with reason: #{inspect(reason)}")
    state[:store_id] |> leave()
  end

  @impl true
  def init(config) do
    timeout = config[:timeout] || 1000
    Logger.debug("#{Themes.cluster(self())} is UP.")
    Process.flag(:trap_exit, true)
    Process.send_after(self(), :join, timeout)
    Process.send_after(self(), :members, 2 * timeout)
    {:ok, config}
  end

  def start_link(opts),
    do:
    GenServer.start_link(
      __MODULE__,
      opts,
      name: __MODULE__
    )

  def child_spec(opts),
    do:
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 10_000,
      type: :worker
    }

end
