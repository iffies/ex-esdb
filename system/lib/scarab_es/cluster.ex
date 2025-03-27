defmodule ExESDB.Cluster do
  @moduledoc false
  use GenServer

  require Logger
  require Colors

  # defp ping?(node) do
  #   case :net_adm.ping(node) do
  #     :pong -> true
  #     _ -> false
  #   end
  # end

  defp join(store) do
      ExESDB.Options.seed_nodes()
      |> Enum.map(
      fn seed -> 
        Logger.info("Joining node #{inspect(seed)} in cluster #{inspect(store)}")
        store 
        |> :khepri_cluster.join(seed) 
      end)
  end

  defp leave(store) do 
    case store |> :khepri_cluster.reset() do
     :ok ->
        Logger.info("left cluster")
        :ok
     {:error, reason} -> 
        Logger.error("Failed to leave cluster. reason: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp members(store)  do 
    case store
      |>:khepri_cluster.members() do
     {:error, reason} -> 
        Logger.error("Failed to get store members. reason: #{inspect(reason)}")
      members -> 
        Logger.info("
        Store members: 
        #{inspect(members, pretty: true)}
        ")
    end
  end


  @impl true
  def handle_info(:join, %{store_id: store} = state) do
    store 
    |> join()
    {:noreply, state}
  end

  @impl true
  def handle_info(:members, %{store_id: store, timeout: timeout} = state) do
    store 
    |> members()
    Process.send_after(self(), :members, 2 * timeout)
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, %{store_id: store} = state) do
    Logger.warning("Cluster #{Colors.cluster_theme(pid)} exited with reason: #{inspect(reason)}")
    store |> leave()
    {:noreply, state}
  end


  ############# PLUMBING #############
  @impl true
  def terminate(reason, %{store_id: store}) do
    IO.puts("#{Colors.cluster_theme(self())} terminating with reason: #{inspect(reason)}")
    store |> leave()
  end

  @impl true
  def init(%{timeout: timeout} = config) do
    Logger.info("#{Colors.cluster_theme(self())} => Starting Cluster with config: #{inspect(config, pretty: true)}")
    Process.flag(:trap_exit, true)
    Process.send_after(self(), :join, timeout)
    Process.send_after(self(), :members, 6 * timeout)
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
