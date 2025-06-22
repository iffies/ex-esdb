defmodule ExESDB.StreamsReaderWorker do
  @moduledoc """
    Provides functions for reading and streaming events from the event store.
  """
  use GenServer
  require Logger
  alias ExESDB.Themes, as: Themes

  alias ExESDB.StreamsHelper, as: Helper
  alias ExESDB.StreamsReader, as: StreamsReader

  import ExESDB.Khepri.Conditions

  ############ CALLBACKS ############
  @impl true
  def handle_call(
        {:stream_events, store, stream_id, start_version, count, direction},
        _from,
        state
      ) do
    if store
       |> Helper.stream_exists?(stream_id) do
      desired_versions = Helper.calculate_versions(start_version, count, direction)

      event_stream =
        desired_versions
        |> Stream.map(fn version ->
          padded_version = Helper.pad_version(version, 6)

          store
          |> :khepri.get!([:streams, stream_id, padded_version])
        end)
        |> Stream.reject(&is_nil/1)

      {:reply, {:ok, event_stream}, state}
    else
      {:reply, {:error, :stream_not_found}, state}
    end
  end

  @impl true
  def handle_call({:get_streams, store}, _from, state) do
    streams =
      store
      |> :khepri.get_many!([
        :streams,
        if_node_exists(exists: true)
      ])
      |> Enum.reduce([], fn {[:streams, stream_id], _stream}, acc -> [stream_id] ++ acc end)

    {:reply, {:ok, streams}, state}
  end

  ################## PlUMBING ##################

  @impl true
  def init({store, stream_id, partition}) do
    Process.flag(:trap_exit, true)
    name = StreamsReader.worker_id(store, stream_id)
    msg = "[#{inspect(name)}] is UP on partition #{inspect(partition)}, joining the cluster."
    IO.puts("#{Themes.streams_reader_worker(msg)}")
    Swarm.register_name(name, self())

    {:ok,
     %{
       worker_name: name,
       store: store,
       stream_id: stream_id,
       node: node(),
       partition: partition
     }}
  end

  def child_spec({store, stream_id, partition}) do
    %{
      id: {StreamsReader.worker_id(store, stream_id), partition},
      start: {__MODULE__, :start_link, [{store, stream_id, partition}]},
      type: :worker,
      restart: :permanent,
      shutdown: 5000
    }
  end

  def start_link({store, stream_id, partition}) do
    GenServer.start_link(
      __MODULE__,
      {store, stream_id, partition},
      name: {:global, StreamsReader.worker_id(store, stream_id)}
    )
  end
end
