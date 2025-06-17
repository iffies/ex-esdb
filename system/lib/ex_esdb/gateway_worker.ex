defmodule ExESDB.GatewayWorker do
  @moduledoc """
    Provides API functions for working with ExESDB
    ## API
      - ack_event/4
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
        {:reply, streams |> Enum.to_list(), state}

      {:error, _reason} ->
        {:reply, [], state}
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
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5000
    }
  end

  def start_link(opts) do
    GenServer.start_link(
      __MODULE__,
      opts,
      name: __MODULE__
    )
  end

  def gateway_worker_name,
    do: {:gateway_worker, node(), :rand.uniform(10_000)}

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)
    name = gateway_worker_name()

    Swarm.register_name(name, self())

    IO.puts("#{Themes.gateway_worker(self())} is UP with name #{inspect(name)}")

    {:ok, opts}
  end

  @impl true
  def terminate(_reason, _state) do
    IO.puts("#{Themes.gateway_worker(self())} TERMINATED")
    :ok
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, state) do
    IO.puts("#{Themes.gateway_worker(self())} EXIT #{inspect(pid)} #{inspect(reason)}")
    {:noreply, state}
  end
end
