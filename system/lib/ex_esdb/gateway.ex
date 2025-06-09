defmodule ExESDB.Gateway do
  @moduledoc """
    Provides API functions for working with ExESDB
    ## API
      - append_events/4
      - get_events/4
      - get_streams/1

      - add_subscription/4
      - subscribe_to/5
      - unsubscribe/2
      - delete_subscription/4

  """
  use GenServer

  alias ExESDB.SubscriptionsReader, as: SubsR
  alias ExESDB.SubscriptionsWriter, as: SubsW

  alias ExESDB.StreamsHelper, as: StreamsH
  alias ExESDB.StreamsReader, as: StreamsR
  alias ExESDB.StreamsWriter, as: StreamsW
  alias ExESDB.Themes, as: Themes

  require Logger

  @type store :: atom()
  @type stream :: String.t()
  @type subscription_name :: String.t()
  @type error :: term
  @type subscription_type :: :by_stream | :by_event_type | :by_event_pattern
  @type selector_type :: String.t() | map()

  def get_subscriptions(store),
    do:
      GenServer.call(
        __MODULE__,
        {:get_subscriptions, store}
      )

  def append_events(store, stream_id, events),
    do:
      GenServer.cast(
        __MODULE__,
        {:append_events, store, stream_id, events}
      )

  def get_events(store, stream_id, start_version, count, direction \\ :forward),
    do:
      GenServer.call(
        __MODULE__,
        {:get_events, store, stream_id, start_version, count, direction}
      )

  @doc """
    Get all streams from the store.
    ## Parameters
      - store: the id of the store
    ## Returns
      - a list of all streams in the store
  """
  @spec get_streams(store :: atom()) :: {:ok, list()} | {:error, term()}
  def get_streams(store),
    do:
      GenServer.call(
        __MODULE__,
        {:get_streams, store}
      )

  @doc """
    Add a subscription.
  """
  @spec add_subscription(
          store :: store,
          type :: subscription_type,
          selector :: selector_type,
          subscription_name :: subscription_name,
          subscriber :: pid | nil,
          start_from :: integer
        ) :: :ok | {:error, error}
  def add_subscription(
        store,
        type,
        selector,
        subscription_name \\ "transient",
        subscriber \\ nil,
        start_from \\ 0
      ),
      do:
        GenServer.cast(
          __MODULE__,
          {:add_subscription, store, type, selector, subscription_name, subscriber, start_from}
        )

  @doc """
    Delete a subscription.
  """
  @spec remove_subscription(
          store :: any,
          type :: subscription_type,
          selector :: selector_type,
          subscription_name :: subscription_name
        ) :: :ok | {:error, error}
  def remove_subscription(store, type, selector, subscription_name \\ "transient"),
    do:
      GenServer.cast(
        __MODULE__,
        {:remove_subscription, store, type, selector, subscription_name}
      )

  ############ HANDLE_CALL ############
  @impl GenServer
  def handle_call({:get_events, store, stream_id, start_version, count, direction}, _from, state) do
    case store
         |> StreamsR.stream_events(stream_id, start_version, count, direction) do
      {:ok, events} ->
        {:reply, {:ok, events |> Enum.to_list()}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl GenServer
  def handle_call({:get_streams, store}, _from, state) do
    case store
         |> StreamsR.get_streams() do
      {:ok, streams} ->
        {:reply, {:ok, streams |> Enum.to_list()}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl GenServer
  def handle_call({:get_subscriptions, store}, _from, state) do
    reply =
      store
      |> SubsR.get_subscriptions()

    {:reply, reply, state}
  end

  ################ HANDLE_CAST #############
  @impl GenServer
  def handle_cast({:append_events, store, stream_id, events}, state) do
    current_version =
      store
      |> StreamsH.get_version!(stream_id)

    store
    |> StreamsW.append_events(stream_id, current_version, events)

    {:noreply, state}
  end

  @impl true
  def handle_cast(
        {:remove_subscription, store, type, selector, subscription_name},
        state
      ) do
    store
    |> SubsW.delete_subscription(type, selector, subscription_name)

    {:noreply, state}
  end

  @impl true
  def handle_cast(
        {:add_subscription, store, type, selector, subscription_name, subscriber, start_from},
        state
      ) do
    store
    |> SubsW.put_subscription(type, selector, subscription_name, start_from, subscriber)

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
    IO.puts("#{Themes.gateway(self())} is UP!")
    {:ok, opts}
  end
end
