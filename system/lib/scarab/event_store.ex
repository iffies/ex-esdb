defmodule Scarab.EventStore do
  @moduledoc """
  A GenServer wrapper around :khepri to act as an event store.
  Inspired by EventStoreDB's API.
  """
  use GenServer

  require Logger

  alias Scarab.EventData
  alias Scarab.NewEvent
  alias Scarab.RecordedEvent
  alias Scarab.SnapshotData
  alias Scarab.TypeProvider
  alias Scarab.EventStreamWriter, as: ESWriter
  alias Scarab.EventStreamReader, as: ESReader

  # Client API
  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  def get_streams(store),
    do:
      GenServer.call(
        __MODULE__,
        {:get_streams, store}
      )

  @doc """
  Append events to a stream.

  ## Parameters

    - `stream_id`: The id of the stream to append to.
    - `expected_version`: The expected version of the stream (for optimistic concurrency).
    - `events`: A list of events to append.

  ## Returns

    - `{:ok, new_stream_version}` if successful.
    - `{:error, reason}` if unsuccessful.
  """
  def append_to_stream(store, stream_id, expected_version, events),
    do:
      GenServer.call(
        __MODULE__,
        {:append_to_stream, store, stream_id, expected_version, events}
      )

  @doc """
  Read events from a stream.

  ## Parameters

    - `stream_name`: The name of the stream to read from.
    - `start_version`: The version to start reading from.
    - `count`: The number of events to read.

  ## Returns

    - `{:ok, events}` if successful.
    - `{:error, reason}` if unsuccessful.
  """
  def read_stream_forward(store, stream_id, start_version, count),
    do:
      GenServer.call(
        __MODULE__,
        {:read_stream_forward, store, stream_id, start_version, count}
      )

  @doc """
  Get the current version of a stream.

  ## Parameters

    - `stream_name`: The name of the stream to check.

  ## Returns

    - `{:ok, version}` if successful.
    - `{:error, reason}` if unsuccessful.
  """
  def stream_version(store, stream_id) do
    GenServer.call(
      __MODULE__,
      {:stream_version, store, stream_id}
    )
  end

  ## CALLBACKS
  #
  @impl true
  def handle_call(
        {:get_streams, store},
        _from,
        state
      ) do
    streams =
      store
      |> :khepri.get!([:streams])

    {:reply, streams, state}
  end

  @impl true
  def handle_call(
        {:append_to_stream, store, stream_id, expected_version, events},
        _from,
        state
      ) do
    current_version =
      store
      |> ESReader.get_current_version(stream_id)

    if current_version == expected_version do
      new_version =
        store
        |> ESWriter.append_events(stream_id, events, current_version)

      {:reply, {:ok, new_version}, state}
    else
      {:reply, {:error, :wrong_expected_version}, state}
    end
  end

  @impl true
  def handle_call(
        {:read_stream_forward, store, stream_id, start_version, count},
        _from,
        state
      ) do
    events =
      store
      |> ESReader.read_events(stream_id, start_version, count)

    {:reply, {:ok, events}, state}
  end

  @impl true
  def handle_call(
        {:stream_version, store, stream_id},
        _from,
        state
      ) do
    version =
      store
      |> ESReader.get_current_version(stream_id)

    {:reply, {:ok, version}, state}
  end

  defp start_khepri(
         %{
           data_dir: data_dir,
           store_id: store_id,
           timeout: timeout,
           db_type: _db_type
         } = opts
       ) do
    Logger.debug("Starting Khepri with config: #{inspect(opts, pretty: true)}")
    :khepri.start(data_dir, store_id, timeout)
  end

  #### PLUMBING
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 10_000,
      type: :worker
    }
  end

  # Server Callbacks
  @impl true
  def init(opts) do
    Logger.debug("Starting Scarab EventStore with config: #{inspect(opts, pretty: true)}")

    case start_khepri(opts) do
      {:ok, store_id} ->
        Logger.debug("Started khepri with store_id: #{inspect(store_id)}")

        {:ok, opts}

      result ->
        Logger.error("Failed to start khepri with result: #{inspect(result)}")
    end
  end
end
