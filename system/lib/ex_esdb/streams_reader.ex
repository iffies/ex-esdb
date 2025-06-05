defmodule ExESDB.StreamsReader do
  @moduledoc """
    Provides functions for reading and streaming events from the event store.
  """
  use GenServer
  require Logger

  alias ExESDB.StreamsHelper, as: Helper

  ########### API ############
  @doc """
    Returns a list of all streams in the store.
    ## Parameters
    #  - `store` is the name of the store.
    ## Returns
    #  - `{:ok, streams}`  if successful.
  """
  @spec get_streams(store :: atom()) :: {:ok, list()} | {:error, term()}
  def get_streams(store),
    do:
      GenServer.call(
        __MODULE__,
        {:get_streams, store}
      )

  @doc """
    Streams events from `stream` in batches of `count` events, in a `direction`.
  """
  @spec stream_events(
          store :: atom(),
          stream_id :: any(),
          start_version :: integer(),
          count :: integer(),
          direction :: :forward | :backward
        ) :: {:ok, Enumerable.t()} | {:error, term()}
  def stream_events(store, stream_id, start_version, count, direction \\ :forward),
    do:
      GenServer.call(
        __MODULE__,
        {:stream_events, store, stream_id, start_version, count, direction}
      )

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
      |> :khepri.get!([:streams])
      |> Enum.reduce([], fn {stream_id, _stream}, acc -> stream_id ++ acc end)

    {:reply, {:ok, streams}, state}
  end

  ################## PlUMBING ##################

  @impl true
  def init(opts) do
    {:ok, opts}
  end

  def child_spec(opts),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5000
    }

  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )
end
