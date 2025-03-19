defmodule Scarab.Cluster do
  @moduledoc false
  use GenServer

  require Logger
  require Colors

  def join(store) do
      Scarab.Config.scarab_seeds()
      |> Enum.map(
      fn seed -> 
        Logger.info("Joining node #{inspect(seed)} in cluster #{inspect(store)}")
        store 
        |> :khepri_cluster.join(seed) 
      end)
  end

  def leave,
    do: :khepri_cluster.reset()

  def members(store),
    do:
      store
      |> :khepri_cluster.members()




  ############# PLUMBING #############
  #
  @impl true
  def init(%{store_id: store, db_type: _db_type} = config) do
    Logger.info("#{Colors.cluster_theme(self())} => Starting Cluster with config: #{inspect(config, pretty: true)}")
    store
    |> join()
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
