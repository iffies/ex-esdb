defmodule ExESDB.SubscriptionsTracker do
  @moduledoc """
    As part of the ExESDB.System, the SubscriptionsTracker is responsible for
    observing the subscriptions that are maintained in the Store.

    Since Khepri triggers are executed on the leader node, the SubscriptionsTracker
    will be instructed to start the Emitters system on the leader node whenever a new subscription
    is registered.

    When a Subscription is deleted, the SubscriptionsTracker will instruct the Emitters system to stop 
    the associated EmitterPool.
  """
  use GenServer

  require Logger
  alias ExESDB.Emitters, as: Emitters
  alias ExESDB.StoreCluster, as: StoreCluster

  ########### HANDLE_INFO ###########
  @impl GenServer
  def handle_info({:feature_created, :subscriptions, data}, state) do
    Logger.info("🔔 Subscription #{inspect(data)} registered", component: :subscriptions_tracker)
    store = state[:store_id]

    if StoreCluster.leader?(store) do
      # Extract subscription data and start emitter pool

      case Emitters.start_emitter_pool(store, data) do
        {:ok, _pid} ->
          Logger.info("✅ Successfully started EmitterPool for subscription #{data.subscription_name}", component: :subscriptions_tracker)

        {:error, {:already_started, _pid}} ->
          Logger.info("ℹ️ EmitterPool already exists for subscription #{data.subscription_name}", component: :subscriptions_tracker)

        {:error, reason} ->
          Logger.error("❌ Failed to start EmitterPool for subscription #{data.subscription_name}: #{inspect(reason)}", component: :subscriptions_tracker)
      end
    end

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:feature_updated, :subscriptions, data}, state) do
    Logger.info("🔄 Subscription #{inspect(data)} updated", component: :subscriptions_tracker)

    if StoreCluster.leader?(state[:store_id]) do
      try do
        Emitters.update_emitter_pool(state[:store_id], data)

        Logger.info("✅ Successfully updated EmitterPool for subscription #{data.subscription_name}", component: :subscriptions_tracker)
      rescue
        error ->
          Logger.error("❌ Failed to update EmitterPool for subscription #{data.subscription_name}: #{inspect(error)}", component: :subscriptions_tracker)
      end
    end

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:feature_deleted, :subscriptions, data}, state) do
    Logger.info("🗑️ Subscription #{inspect(data)} deleted", component: :subscriptions_tracker)

    if StoreCluster.leader?(state[:store_id]) do
      try do
        Emitters.stop_emitter_pool(state[:store_id], data)

        Logger.info("🛑 Successfully stopped EmitterPool for subscription #{data.subscription_name}", component: :subscriptions_tracker)
      rescue
        error ->
          Logger.error("❌ Failed to stop EmitterPool for subscription #{data.subscription_name}: #{inspect(error)}", component: :subscriptions_tracker)
      end
    end

    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    Logger.warning("🚨 LeaderTracker #{inspect(pid)} exited with reason: #{inspect(reason)}", component: :leader_tracker)
    store = state[:store_id]

    store
    |> :tracker_group.leave(:subscriptions, self())

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(_, state) do
    {:noreply, state}
  end

  ############## PLUMBING ##############
  @impl GenServer
  def init(opts) do
    Process.flag(:trap_exit, true)
    store = Keyword.get(opts, :store_id)
    Logger.info("📊 LeaderTracker #{inspect(self())} is UP.", component: :leader_tracker)

    :ok =
      store
      |> :subscriptions.setup_tracking(self())

    {:ok, opts}
  end

  @impl true
  def terminate(reason, state) do
    Logger.info("🔴 LeaderTracker #{inspect(self())} terminating with reason: #{inspect(reason)}", component: :leader_tracker)
    store = state[:store_id]

    store
    |> :tracker_group.leave(:subscriptions, self())

    :ok
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
