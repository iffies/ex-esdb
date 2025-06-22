defmodule ExESDB.EmitterWorker do
  @moduledoc """
    As part of the ExESDB.System, 
    the EmitterWorker is responsible for managing the communication 
    between the Event Store and the PubSub mechanism.
  """
  use GenServer

  alias ExESDB.Options, as: Options
  alias Phoenix.PubSub, as: PubSub

  require ExESDB.Themes, as: Themes

  require Logger

  defp send_or_kill(pid, event, store, selector) do
    if Process.alive?(pid) do
      Process.send(pid, {:event_emitted, event}, [])
    else
      ExESDB.EmitterPool.stop(store, selector)
    end
  end

  defp emit(pub_sub, topic, event),
    do:
      pub_sub
      |> PubSub.broadcast(topic, {:event_emitted, event})

  @impl GenServer
  def init({store, sub_topic, subscriber}) do
    scheduler_id = :erlang.system_info(:scheduler_id)
    topic = :emitter_group.topic(store, sub_topic)
    :ok = :emitter_group.join(store, sub_topic, self())

    IO.puts(
      "#{Themes.emitter_worker(self())} for #{inspect(topic)} is UP on scheduler #{inspect(scheduler_id)}"
    )

    {:ok, %{subscriber: subscriber, store: store, selector: sub_topic}}
  end

  @impl GenServer
  def terminate(reason, %{store: store, selector: selector}) do
    IO.puts("#{Themes.emitter_worker(self())} is TERMINATED with reason #{inspect(reason)}")
    :ok = :emitter_group.leave(store, selector, self())
    :ok
  end

  def start_link({store, sub_topic, subscriber, emitter}),
    do:
      GenServer.start_link(
        __MODULE__,
        {store, sub_topic, subscriber},
        name: emitter
      )

  @impl true
  def handle_info(
        {:broadcast, topic, event},
        %{subscriber: subscriber, store: store, selector: selector} = state
      ) do
    case subscriber do
      nil ->
        pubsub = Options.pub_sub()

        pubsub
        |> emit(topic, event)

      pid ->
        send_or_kill(pid, event, store, selector)
    end

    {:noreply, state}
  end

  @impl true
  def handle_info(
        {:forward_to_local, topic, event},
        %{subscriber: subscriber, store: store, selector: selector} = _state
      ) do
    case subscriber do
      nil ->
        pubsub = Options.pub_sub()

        pubsub
        |> emit(topic, event)

      pid ->
        send_or_kill(pid, event, store, selector)
    end

    {:noreply, subscriber}
  end

  @impl true
  def handle_info(_, state) do
    {:noreply, state}
  end
end
