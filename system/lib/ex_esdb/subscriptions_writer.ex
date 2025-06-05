defmodule ExESDB.SubscriptionsWriter do
  @moduledoc """
   Provides functions for working with event store subscriptions.
  """
  use GenServer

  alias ExESDB.SubscriptionsHelper, as: Helper

  # @spec put_subscription(
  #         store :: atom(),
  #         type :: :by_stream | :by_event_type | :by_event_pattern,
  #         selector :: String.t() | Enumerable.t(),
  #         subscription_name :: String.t(),
  #         start_from :: integer(),
  #         subscriber :: pid()
  #       ) :: any()
  def put_subscription(
        store,
        type,
        selector,
        subscription_name \\ "transient",
        start_from \\ 0,
        subscriber \\ nil
      ),
      do:
        GenServer.call(
          __MODULE__,
          {:put_subscription, store, type, selector, subscription_name, start_from, subscriber}
        )

  def delete_subscription(store, type, selector, subscription_name),
    do:
      GenServer.cast(
        __MODULE__,
        {:delete_subscription, store, type, selector, subscription_name}
      )

  ############ CALLBACKS ############
  @impl true
  def handle_cast({:delete_subscription, store, type, selector, subscription_name}, state) do
    key = Helper.subscriptions_key(type, selector, subscription_name)

    if store
       |> :khepri.exists!([:subscriptions, key]) do
      store
      |> :khepri.delete!([:subscriptions, key])
    end

    {:noreply, state}
  end

  @impl true
  def handle_call(
        {:put_subscription, store, type, selector, subscription_name, start_from, subscriber},
        _from,
        state
      ) do
    key =
      Helper.subscriptions_key(type, selector, subscription_name)

    subscription =
      %{
        selector: selector,
        type: type,
        subscription_name: subscription_name,
        start_from: start_from,
        subscriber: subscriber
      }

    if not store
       |> :khepri.exists!([:subscriptions, key]) do
      store
      |> :khepri.put(
        [:subscriptions, key],
        subscription
      )
    end

    {:reply, subscription, state}
  end

  ######## PLUMBING ############
  @impl true
  def init(opts) do
    {:ok, opts}
  end

  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  def child_spec(opts),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5000
    }
end
