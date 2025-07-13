defmodule ExESDB.SnapshotsWriterWorker do
  @moduledoc """
    A worker process for writing snapshots to the event store.
  """
  use GenServer

  alias ExESDB.Snapshots, as: Snapshots
  alias ExESDB.SnapshotsWriter, as: SnapshotsWriter

  require Logger

  ################ PLUMBING ################
  @doc """
   Starts a new `ExESDB.SnapshotsWriterWorker` process.
  """
  def start_link({store, source_uuid, stream_uuid, partition}) do
    GenServer.start_link(
      __MODULE__,
      {store, source_uuid, stream_uuid, partition},
      name: SnapshotsWriter.hr_snapshots_writer_name(store, source_uuid, stream_uuid)
    )
  end

  @impl true
  def init({store, source_uuid, stream_uuid, partition}) do
    cluster_id = SnapshotsWriter.cluster_id(store, source_uuid, stream_uuid)
    Swarm.register_name(cluster_id, self())
    msg = "[🔷📸] [#{inspect(self())}][SnapshotsWriterWorker] [#{inspect(self())}] is UP on partition #{inspect(partition)}, joining the cluster."
    Logger.info(msg, component: :snapshots_writer_worker, pid: self())
    {:ok, {store, source_uuid, stream_uuid, partition}}
  end

  ################ IMPLEMENTATION ################
  @impl true
  def handle_cast({:delete_snapshot, store, source_uuid, stream_uuid, version}, state) do
    path = Snapshots.path(source_uuid, stream_uuid, version)

    store
    |> :khepri.delete!(path)

    {:noreply, state}
  end

  @impl true
  def handle_cast(
        {:record_snapshot, store, source_uuid, stream_uuid, version, snapshot_record},
        state
      ) do
    path = Snapshots.path(source_uuid, stream_uuid, version)

    store
    |> :khepri.put!(path, snapshot_record)

    {:noreply, state}
  end
end
