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

  # Client API
  def start_link(%{store_id: store_id} = opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: Module.concat(__MODULE__, store_id)
      )

  def get_state(store_id) do
    GenServer.call(
      Module.concat(__MODULE__, store_id),
      {:get_state}
    )
  end

  @doc """
  Append events to a stream.

  ## Parameters

    - `stream_name`: The name of the stream to append to.
    - `expected_version`: The expected version of the stream (for optimistic concurrency).
    - `events`: A list of events to append.

  ## Returns

    - `{:ok, new_stream_version}` if successful.
    - `{:error, reason}` if unsuccessful.
  """
  def append_to_stream(store_id, stream_name, expected_version, events) do
    GenServer.call(
      Module.concat(__MODULE__, store_id),
      {:append_to_stream, stream_name, expected_version, events}
    )
  end

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
  def read_stream_forward(store_id, stream_name, start_version, count) do
    GenServer.call(
      Module.concat(__MODULE__, store_id),
      {:read_stream_forward, stream_name, start_version, count}
    )
  end

  @doc """
  Get the current version of a stream.

  ## Parameters

    - `stream_name`: The name of the stream to check.

  ## Returns

    - `{:ok, version}` if successful.
    - `{:error, reason}` if unsuccessful.
  """
  def stream_version(store_id, stream_name) do
    GenServer.call(
      Module.concat(__MODULE__, store_id),
      {:stream_version, stream_name}
    )
  end

  ## CALLBACKS
  @impl true
  def handle_call({:append_to_stream, stream_name, expected_version, events}, _from, state) do
    path = [:streams, stream_name]
    current_version = get_current_version(path)

    if current_version == expected_version do
      new_version = append_events(path, events, current_version)
      {:reply, {:ok, new_version}, state}
    else
      {:reply, {:error, :wrong_expected_version}, state}
    end
  end

  @impl true
  def handle_call({:read_stream_forward, stream_name, start_version, count}, _from, state) do
    path = [:streams, stream_name]
    events = read_events(path, start_version, count)
    {:reply, {:ok, events}, state}
  end

  @impl true
  def handle_call({:stream_version, stream_name}, _from, state) do
    path = [:streams, stream_name]
    version = get_current_version(path)
    {:reply, {:ok, version}, state}
  end

  @impl true
  def handle_call({:get_state}, _from, state) do
    {:reply, state, state}
  end

  # Helper functions
  defp get_current_version(path) do
    case :khepri.get(path) do
      {:ok, {_, metadata}} -> metadata[:version] || 0
      _ -> 0
    end
  end

  defp append_events(path, events, current_version) when is_list(path) do
    events
    |> Enum.reduce(
      current_version,
      fn event, version ->
        new_version = version + 1
        :khepri.put(path ++ new_version, event)
        new_version
      end
    )
  end

  defp read_events(path, start_version, count) when is_list(path) do
    start_version..(start_version + count - 1)
    |> Enum.map(fn version ->
      case :khepri.get(path ++ version) do
        {:ok, {_, event}} -> event
        _ -> nil
      end
    end)
    |> Enum.reject(&is_nil/1)
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
  def child_spec(%{store_id: store_id} = opts) do
    %{
      id: Module.concat(__MODULE__, store_id),
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 500,
      type: :worker
    }
  end

  # Server Callbacks
  @impl true
  def init(opts) do
    Logger.debug("Starting Scarab EventStore with config: #{inspect(opts, pretty: true)}")
    {:ok, start_khepri(opts)}
  end
end
