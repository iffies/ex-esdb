defmodule ExESDB.Gateway do
  @moduledoc """
   Provides API functions for working with ExESDB
  """
  use GenServer

  alias ExESDB.SubscriptionsWriter, as: Writer

  alias ExESDB.StreamsHelper, as: StreamsHelper
  alias ExESDB.StreamsReader, as: StreamsReader
  alias ExESDB.StreamsWriter, as: StreamsWriter

  alias ExESDB.Emitters
  require Logger

  @type store :: atom()
  @type stream :: String.t()
  @type subscription_name :: String.t()
  @type error :: term
  @type subscription_type :: :by_stream | :by_event_type | :by_event_pattern
  @type selector_type :: String.t() | map()

  def append_events(store, stream_id, events),
    do:
      GenServer.cast(
        __MODULE__,
        {:append_events, store, stream_id, events}
      )

  def get_events(store, stream_id, start_version, count, direction),
    do:
      GenServer.call(
        __MODULE__,
        {:get_events, store, stream_id, start_version, count, direction}
      )

  def get_streams(store),
      GenServer.call(
        __MODULE__,
        {:get_streams, store}
      )

  @doc """
    Create a transient subscription for a specific stream or for all streams.
  """
  @spec subscribe(
          store :: store,
          type :: subscription_type,
          selector :: selector_type,
          subscription_name :: String.t()
        ) :: :ok | {:error, error}
  def subscribe(store, type, selector, subscription_name \\ "transient"),
    do:
      GenServer.cast(
        __MODULE__,
        {:subscribe, store, type, selector, subscription_name}
      )

  @spec subscribe_to(
          store :: store,
          type :: subscription_type,
          selector :: selector_type,
          subscription_name :: subscription_name,
          subscriber :: pid,
          start_from :: integer
        ) :: :ok | {:error, error}
  def subscribe_to(
        store,
        type,
        selector,
        subscription_name,
        subscriber,
        start_from \\ 0
      ),
      do:
        GenServer.cast(
          __MODULE__,
          {:subscribe_to, store, type, selector, subscription_name, subscriber, start_from}
        )

  @doc """
    Unsubscribe from a subscription.
  """
  def unsubscribe(store, subscription_name),
    do:
      GenServer.cast(
        __MODULE__,
        {:unsubscribe, store, subscription_name}
      )

  @doc """
    Delete a subscription.
  """
  @spec delete_subscription(
          store :: any,
          type :: subscription_type,
          selector :: selector_type,
          subscription_name :: subscription_name
        ) :: :ok | {:error, error}
  def delete_subscription(store, type, selector \\ "$all", subscription_name \\ "transient"),
    do:
      GenServer.cast(
        __MODULE__,
        {:delete_subscription, store, type, selector, subscription_name}
      )

  ############ HANDLE_CALL ############
  #
  @impl GenServer
  def handle_call({:get_events, store, stream_id, start_version, count, direction}, _from, state) do
    case store
         |> StreamsReader.stream_events(stream_id, start_version, count, direction) do
      {:ok, events} ->
        {:reply, {:ok, events |> Enum.to_list()}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl GenServer
  def handle_call({:get_streams, store}, _from, state) do
    case store
         |> StreamsReader.get_streams() do
      {:ok, streams} ->
        {:reply, {:ok, streams |> Enum.to_list()}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  ################ HANDLE_CAST #############
  @impl GenServer
  def handle_cast({:append_events, store, stream_id, events}, state) do
    current_version =
      StreamsHelper.get_version!(store, stream_id)

    StreamsWriter.append_events(store, stream_id, current_version, events)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:unsubscribe, store, subscription_name}, state) do
    store
    |> :khepri.delete!([:subscriptions, subscription_name])

    {:noreply, state}
  end

  @impl true
  def handle_cast(
        {:delete_subscription, store, type, selector, subscription_name},
        state
      ) do
    store
    |> Writer.delete_subscription(type, selector, subscription_name)

    {:noreply, state}
  end

  @impl true
  def handle_cast(
        {:subscribe_to, store, type, selector, subscription_name, subscriber, start_from},
        state
      ) do
    store
    |> Writer.put_subscription(type, selector, subscription_name, start_from, subscriber)
    |> Emitters.start_emitter()

    {:noreply, state}
  end

  @impl true
  def handle_cast({:subscribe, store, type, selector, subscription_name}, state) do
    store
    |> Writer.put_subscription(type, selector, subscription_name)
    |> ExESDB.Emitters.start_emitter()

    {:noreply, state}
  end

  ############# PLUMBING #############

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

  @impl true
  def init(opts) do
    {:ok, opts}
  end
end
