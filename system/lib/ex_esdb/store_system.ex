defmodule ExESDB.StoreSystem do
  @moduledoc """
  Supervisor for store-related components.
  
  This supervisor manages the store lifecycle and clustering components.
  Uses :rest_for_one strategy because StoreCluster depends on Store.
  
  Components:
  - Store: Core store GenServer
  - StoreCluster: Clustering coordination GenServer
  """
  use Supervisor
  
  require Logger

  @impl true
  def init(opts) do
    children = [
      # Store must start first as StoreCluster depends on it
      {ExESDB.Store, opts},
      {ExESDB.StoreCluster, opts}
    ]

    Logger.info("🏗️ StoreSystem #{inspect(self())} is UP", component: :system)
    
    # Use :rest_for_one because StoreCluster depends on Store
    Supervisor.init(children, 
      strategy: :rest_for_one,
      max_restarts: 5,
      max_seconds: 30
    )
  end

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: :infinity,
      type: :supervisor
    }
  end
end
