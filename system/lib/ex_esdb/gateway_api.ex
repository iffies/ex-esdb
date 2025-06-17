defmodule ExESDB.GatewayAPI do
  @moduledoc """
    Provides API functions for working with ExESDB
  """
  @type store :: atom()
  @type stream :: String.t()
  @type subscription_name :: String.t()
  @type error :: term
  @type subscription_type :: :by_stream | :by_event_type | :by_event_pattern
  @type selector_type :: String.t() | map()

  use GenServer
  require Logger
  alias ExESDB.Themes, as: Themes

  def gateway_worker_pids,
    do:
      Swarm.registered()
      |> Enum.filter(fn {name, _} -> match?({:gateway_worker, _, _}, name) end)
      |> Enum.map(fn {_, pid} -> pid end)

  def random_gateway_worker,
    do:
      gateway_worker_pids()
      |> Enum.random()

  def get_subscriptions(store),
    do:
      GenServer.call(
        random_gateway_worker(),
        {:get_subscriptions, store}
      )

  def ack_event(store, subscriber_pid, event, opts),
    do:
      GenServer.cast(
        random_gateway_worker(),
        {:ack_event, store, subscriber_pid, event, opts}
      )

  def append_events(store, stream_id, events),
    do:
      GenServer.cast(
        random_gateway_worker(),
        {:append_events, store, stream_id, events}
      )

  def get_events(store, stream_id, start_version, count, direction \\ :forward),
    do:
      GenServer.call(
        random_gateway_worker(),
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
        random_gateway_worker(),
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
          random_gateway_worker(),
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
        random_gateway_worker(),
        {:remove_subscription, store, type, selector, subscription_name}
      )

  ################## PLUMBING ##################
  def init(opts) do
    IO.puts("#{Themes.gateway_api(self())} is UP!")
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
