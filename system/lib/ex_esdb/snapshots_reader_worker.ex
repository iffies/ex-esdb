defmodule ExESDB.SnapshotsReaderWorker do
  @moduledoc """
    A worker process for reading snapshots from the event store.
  """
  use GenServer

  alias ExESDB.Snapshots, as: Snapshots
  alias ExESDB.SnapshotsReader, as: SnapshotsReader

  alias ExESDB.Themes, as: Themes

  ################ PLUMBING ################
  def start_link({store, source_uuid, stream_uuid, partition}) do
    GenServer.start_link(
      __MODULE__,
      {store, source_uuid, stream_uuid, partition},
      name: SnapshotsReader.hr_snapshots_reader_name(store, source_uuid, stream_uuid)
    )
  end

  @impl true
  def init({store, source_uuid, stream_uuid, partition}) do
    cluster_id = SnapshotsReader.cluster_id(store, source_uuid, stream_uuid)
    msg = "[#{inspect(self())}] is UP on partition #{inspect(partition)}, joining the cluster."
    Swarm.register_name(cluster_id, self())
    IO.puts("#{Themes.snapshots_reader_worker(msg)}")
    {:ok, {store, source_uuid, stream_uuid, partition}}
  end

  ################ IMPLEMENTATION ################
  @impl true
  def handle_call({:read_snapshot, store, source_uuid, stream_uuid, version}, _from, state) do
    path = Snapshots.path(source_uuid, stream_uuid, version)

    case store
         |> :khepri.get(path) do
      {:ok, :undefined} ->
        {:reply, {:error, :not_found}, state}

      {:ok, snapshot} ->
        {:reply, {:ok, snapshot}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
end
