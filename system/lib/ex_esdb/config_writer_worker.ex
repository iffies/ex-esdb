defmodule ExESDB.ConfigWriterWorker do
  @moduledoc """
  A worker process for writing store configurations to the config store.
  """
  use GenServer

  alias ExESDB.ConfigSystem, as: ConfigSystem
  alias ExESDB.ConfigWriter, as: ConfigWriter
  alias ExESDB.Themes, as: Themes

  ################ PLUMBING ################
  def start_link({store_id, partition}) do
    GenServer.start_link(
      __MODULE__,
      {store_id, partition},
      name: ConfigWriter.hr_config_writer_name(store_id)
    )
  end

  @impl true
  def init({store_id, partition}) do
    cluster_id = ConfigWriter.cluster_id(store_id)
    Swarm.register_name(cluster_id, self())
    IO.puts("#{Themes.config_writer_worker(self(), "is UP on partition #{inspect(partition)}, joining the cluster.")}") 
    {:ok, {store_id, partition}}
  end

  ################ IMPLEMENTATION ################
  @impl true
  def handle_cast({:write_store_config, store_id, config}, state) do
    path = ConfigSystem.path(store_id)
    
    # Read existing config to handle versioning
    {version, existing_metadata} = case :khepri.get(:ex_esdb_config, path) do
      {:ok, existing} when is_map(existing) ->
        existing_version = get_in(existing, [:metadata, :version]) || 0
        metadata = get_in(existing, [:metadata]) || %{}
        {existing_version + 1, metadata}
      _ ->
        {1, %{}}
    end
    
    enhanced_config = %{
      store_id: store_id,
      config: config,
      metadata: Map.merge(existing_metadata, %{
        updated_at: DateTime.utc_now(),
        version: version,
        updated_by: "system"
      }) |> Map.put_new(:created_at, DateTime.utc_now()) |> Map.put_new(:created_by, "system"),
      status: :active
    }

    :khepri.put!(:ex_esdb_config, path, enhanced_config)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:delete_store_config, store_id}, state) do
    path = ConfigSystem.path(store_id)

    :khepri.delete!(:ex_esdb_config, path)

    {:noreply, state}
  end
end
