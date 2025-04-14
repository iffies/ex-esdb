defmodule ExESDB.Subscriptions do
  @moduledoc """
   Provides functions for working with event store subscriptions.
  """
  @type subscription_name :: String.t()
  @type error :: term

  alias ExESDB.EventStore, as: Store

  use GenServer

  def subscribe_to(store, stream_uuid, subscription_name, subscriber, start_from, opts \\ []) do
    Store.subscribe_to(store, stream_uuid, subscription_name, subscriber, start_from, opts)
  end

  def unsubscribe(store, subscription_name) do
    Store.unsubscribe(store, subscription_name)
  end

  @doc """
    Delete a subscription.
  """
  @spec delete_subscription(
          store :: any,
          subscription_name :: subscription_name
        ) :: :ok | {:error, error}
  def delete_subscription(store, subscription_name) do
    case store
         |> :khepri.delete!([:subscriptions, subscription_name]) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  ####### PLUMBING #######
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

  @impl true
  def init(opts) do
    {:ok, opts}
  end
end
