defmodule Scarab.EventStore do
  @moduledoc """
  A GenServer wrapper around :khepri to act as an event store.
  Inspired by EventStoreDB's API.
  """
  use GenServer

  require Logger

  alias Scarab.EventEmitter, as: ESEmitter
  alias Scarab.EventStreamReader, as: ESReader
  alias Scarab.EventStreamWriter, as: ESWriter

  # Client API
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


  @impl true
  def handle_info(:sigterm, %{store_id: store}) do
    Logger.info("#{Colors.store_theme(self())} => SIGTERM received. Resetting cluster #{inspect(store, pretty: true)}")
    {:stop, :normal, nil}
  end

  @impl true
  def handle_info(:register_emitter, [config: _, store: store] = state) do
    store
    |> ESEmitter.register_erl_emitter()
    {:noreply, state}
  end

  ## CALLBACKS
  @impl true
  def handle_call(
        {:get_streams, store},
        _from,
        state
      ) do
    streams =
      store
      |> ESReader.get_streams()

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
      |> ESReader.get_current_version!(stream_id)

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
      |> ESReader.get_current_version!(stream_id)

    {:reply, {:ok, version}, state}
  end

  defp start_khepri(%{
         data_dir: data_dir,
         store_id: store,
         timeout: timeout,
         db_type: _db_type
       }) do
    case :khepri.start(data_dir, store, timeout) do
      {:ok, store} ->
        Logger.info("#{Colors.store_theme(self())} => Started store: #{inspect(store, pretty: true)}")

        #store
        #|> ESEmitter.register_emitter()
 
        {:ok, store}

      reason ->
        Logger.error("Failed to start khepri. reason: #{inspect(reason)}")
    end
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

  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  # Server Callbacks
  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)
    Process.send_after(self(), :register_emitter, 10_000)

    IO.puts("Starting Scarab EventStore with config: #{inspect(opts, pretty: true)}")

    case start_khepri(opts) do
      {:ok, store} ->
        Logger.debug("Started store: #{inspect(store)}")
        :os.set_signal(:sigterm, :handle)
        
        {:ok, [config: opts, store: store]}

      reason ->
        Logger.error("Failed to start khepri. reason: #{inspect(reason)}")

        {:error, [config: opts, store: nil]}
    end
  end
end
