defmodule ExESDB.PersistentEmitterWorker do
  @moduledoc """
    As part of the ExESDB.System,
    the PersistendEmitter is responsible for maintaining the streanm of events
    as defined by a Persistent Subscription.
    It works as follows:
      When a Leader is elected, The SubscriptionsTracker will 
      start a PersistentEmitterPool for each persistent subscription.
      The resulting PersistentEmitterWorkers will then be responsible for:
       - catching up with the event stream, from the last acknowledged event
       - forwarding events to the subscriber
       - registering acknowledgements
       
        
       

  where the subscription_name defers from 
  """
  use GenServer

  require ExESDB.Themes, as: Themes

  require Logger

  alias ExESDB.StreamsReader, as: SReader

  def catch_up(subscription_name),
    do:
      GenServer.cast(
        via_swarm(subscription_name),
        {:catch_up}
      )

  def acknowledge_event(subscription_name, event),
    do:
      GenServer.cast(
        via_swarm(subscription_name),
        {:acknowledge_event, event}
      )

  @impl GenServer
  def handle_info({:event_emitted, event}, state) do
    subscription = Keyword.get(state, :subscription)
    %{subscriber: subscriber_pid} = subscription
    Process.send(subscriber_pid, {:event_emitted, event}, [])
    {:noreply, state}
  end

  @impl GenServer
  def handle_cast({:catch_up}, state) do
    store = Keyword.get(state, :store)
    subscription = Keyword.get(state, :subscription)

    %{start_from: start_from, subscriber: subscriber_pid, selector: selector} = subscription

    stream_id =
      selector
      |> String.slice(1, String.length(selector))

    store
    |> SReader.stream_events(stream_id, start_from, 100, :forward)
    |> Enum.each(fn event ->
      Process.send(subscriber_pid, {:event_emitted, event}, [])
    end)

    {:noreply, state}
  end

  def via_swarm(subscription_name),
    do: {:via, :swarm, subscription_name}

  ############## PLUMBING ##############
  @impl GenServer
  def init({store, subscription}) do
    %{subscription_name: id} = subscription
    scheduler_id = :erlang.system_info(:scheduler_id)
    topic = :emitter_group.topic(store, id)
    :ok = :emitter_group.join(store, id, self())

    IO.puts(
      "#{Themes.persistent_emitter_worker(self())} for #{inspect(topic, pretty: true)} is UP on scheduler #{inspect(scheduler_id)}"
    )

    state = [
      store: store,
      subscription: subscription
    ]

    {:ok, state}
  end

  def child_spec({store, subscripton}),
    do: %{
      id: store,
      start: {__MODULE__, :start_link, [{store, subscripton}]},
      restart: :transient,
      shutdown: 5000
    }

  def start_link({store, %{subscription_name: subscription_name} = subscription}),
    do:
      GenServer.start_link(
        __MODULE__,
        {store, subscription},
        name: :"#{store}:#{subscription_name}_persistent_emitter"
      )
end
