defmodule ExESDB.ConfigReaderWorker do
  @moduledoc """
  A worker process for reading store configurations from the config store.
  """
  use GenServer

  alias ExESDB.ConfigSystem, as: ConfigSystem
  alias ExESDB.ConfigReader, as: ConfigReader
  alias ExESDB.Themes, as: Themes

  import ExESDB.Khepri.Conditions

  ################ PLUMBING ################
  def start_link({store_id, partition}) do
    GenServer.start_link(
      __MODULE__,
      {store_id, partition},
      name: ConfigReader.hr_config_reader_name(store_id)
    )
  end

  @impl true
  def init({store_id, partition}) do
    cluster_id = ConfigReader.cluster_id(store_id)
    Swarm.register_name(cluster_id, self())
    IO.puts("#{Themes.config_reader_worker(self(), "is UP on partition #{inspect(partition)}, joining the cluster.")}") 
    {:ok, {store_id, partition}}
  end

  ################ IMPLEMENTATION ################
  @impl true
  def handle_call({:read_store_config, store_id}, _from, state) do
    path = ConfigSystem.path(store_id)

    case :khepri.get(:ex_esdb_config, path) do
      {:ok, :undefined} ->
        {:reply, {:error, :not_found}, state}

      {:ok, config} ->
        {:reply, {:ok, config}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:list_store_configs}, _from, state) do
    query = [:stores, :config, if_path_matches(regex: :any)]

    case :khepri.get_many(:ex_esdb_config, query) do
      {:ok, configs} ->
        {:reply, {:ok, configs}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
end
