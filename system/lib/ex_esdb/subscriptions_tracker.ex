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

  require ExESDB.Themes, as: Themes

  ########### HANDLE_INFO ###########
  @impl GenServer
  def handle_info({:subscription_created, subscription}, state) do
    IO.puts("Subscription #{inspect(subscription)} registered")
    # TODO: Start Emitters
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:subscription_deleted, subscription}, state) do
    IO.puts("Subscription #{inspect(subscription)} deleted")
    # TODO: Stop Emitters
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    IO.puts("#{Themes.subscriptions_tracker(pid)} exited with reason: #{inspect(reason)}")
    store = state[:store_id]

    store
    |> :tracker_group.leave(self())

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
    store = opts[:store_id]
    IO.puts("#{Themes.subscriptions_tracker(self())} is UP.")

    store
    |> :subscriptions.setup_tracking(self())

    {:ok, opts}
  end

  @impl true
  def terminate(reason, _state) do
    Logger.warning(
      "#{Themes.subscriptions_tracker(self())} terminating with reason: #{inspect(reason)}"
    )

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
