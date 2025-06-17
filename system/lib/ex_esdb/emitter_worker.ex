defmodule ExESDB.EmitterWorker do
  @moduledoc """
    As part of the ExESDB.System, 
    the EmitterWorker is responsible for managing the communication 
    between the Event Store and the PubSub mechanism.
  """
  use GenServer

  alias Phoenix.PubSub, as: PubSub

  require ExESDB.Themes, as: Themes

  require Logger

  defp emit(pub_sub, topic, event),
    do:
      pub_sub
      |> PubSub.broadcast(topic, {:event_emitted, event})

  @impl GenServer
  def init({store, id, pubsub}) do
    scheduler_id = :erlang.system_info(:scheduler_id)
    topic = :emitter_group.topic(store, id)
    :ok = :emitter_group.join(store, id, self())

    IO.puts(
      "#{Themes.emitter_worker(self())} for #{inspect(topic, pretty: true)} is UP on scheduler #{inspect(scheduler_id)}"
    )

    {:ok, pubsub}
  end

  def start_link({store, id, pubsub, emitter}),
    do:
      GenServer.start_link(
        __MODULE__,
        {store, id, pubsub},
        name: emitter
      )

  @impl true
  def handle_info({:broadcast, topic, event}, pubsub) do
    pubsub
    |> emit(topic, event)

    Logger.warning(
      "#{Themes.emitter_worker(self())} BROADCAST #{inspect(event, pretty: true)} => #{inspect(topic, pretty: true)}"
    )

    {:noreply, pubsub}
  end

  @impl true
  def handle_info({:forward_to_local, topic, event}, pubsub) do
    %{event_type: event_type, event_stream_id: stream_id} = event

    pubsub
    |> emit(topic, event)

    msg = "FORWARD"

    IO.puts(
      "#{Themes.emitter_worker(msg)} #{inspect(stream_id, pretty: true)}:#{inspect(event_type, pretty: true)} => #{inspect(topic, pretty: true)}"
    )

    {:noreply, pubsub}
  end

  @impl true
  def handle_info(_, pubsub) do
    {:noreply, pubsub}
  end
end
