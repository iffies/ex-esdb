defmodule ExESDB.EventStore do
  @moduledoc """
  A GenServer wrapper around :khepri to act as an event store.
  Inspired by EventStoreDB's API.
  """
  use GenServer

  require Logger

  alias ExESDB.EventEmitter, as: ESEmitter
  alias ExESDB.EventStreamReader, as: ESReader
  alias ExESDB.EventStreamWriter, as: ESWriter
  alias ExESDB.EventStoreInfo, as: ESInfo
  alias ExESDB.Themes, as: Themes

  # Client API
  @doc """
  Get the list of streams in the store.
  ## Parameters

    - `store`: The store to get the streams from.

  ## Returns

    - `{:ok, streams}`  if successful.

  """
  def get_streams(store),
    do:
      GenServer.call(
        __MODULE__,
        {:get_streams, store}
      )

  @doc """
  Get the current state of the store.
  ## Parameters

    - `store`: The store to get the state of.
 
  ## Returns
 
      - `{:ok, state}`  if successful.
      - `{:error, reason}` if unsuccessful.

  """
  def get_state(_store),
    do:
      GenServer.call(
      __MODULE__,
      {:get_state}
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


  @impl true
  def handle_info(:sigterm, %{store_id: store}) do
    Logger.info("#{Themes.store(self())} => SIGTERM received. Leaving cluster")
    {:stop, :normal, nil}
  end

  @impl true
  def handle_info(:register_emitter, [config: opts, store: store] = state) do
    store
    |> ESEmitter.register_erl_emitter(opts[:pub_sub])
    {:noreply, state}
  end

  ## CALLBACKS
  @impl true
  def handle_call({:get_state}, _from, state) do
    {:reply, {:ok, state}, state}
  end

  @impl true
  def handle_call(
        {:get_streams, store},
        _from,
        state
      ) do
    streams =
      store
      |> ESReader.get_streams()

    {:reply, {:ok, streams}, state}
  end
  @impl true
  def handle_call(
        {:append_to_stream, store, stream_id, expected_version, events},
        _from,
        state
      ) do
    current_version =
      store
      |> ESInfo.get_version!(stream_id)

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
      |> ESInfo.get_version!(stream_id)
    {:reply, {:ok, version}, state}
  end

  defp start_khepri(opts) do
    store = opts[:store_id]
    timeout = opts[:timeout]
    data_dir = opts[:data_dir]
    case :khepri.start(data_dir, store, timeout) do
      {:ok, store} ->
        Logger.info("#{Themes.store(self())} => Started Khepri store: #{inspect(store, pretty: true)}")
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
    Logger.info("#{Themes.store(self())} is UP.")
    Process.flag(:trap_exit, true)
    Process.send_after(self(), :register_emitter, 10_000)

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
