defmodule ExESDB.Repl.Consumer do
  @moduledoc false
  use GenServer

  alias ExESDB.Themes, as: Themes

  @impl true
  def handle_info({:event_emitted, event}, state) do
    %{
      event_stream_id: stream_id,
      event_type: event_type,
      event_number: version,
      data: payload
    } = event

    IO.puts("\nCONSUMED #{stream_id}:#{event_type} 
         version #{inspect(version)}
         payload: #{inspect(payload, pretty: true)}")

    {:noreply, state}
  end

  @impl true
  def handle_info(_, state) do
    {:noreply, state}
  end

  ############## PLUMBING ##############
  @impl true
  def init(args) do
    topic = Keyword.get(args, :topic, "reg_gh:$all")
    pubsub = Keyword.get(args, :pubsub, :ex_esdb_pubsub)

    pubsub
    |> Phoenix.PubSub.subscribe(topic)

    {:ok, args}
  end

  def start_link(args) do
    topic = Keyword.get(args, :topic, "reg_gh:$all")

    GenServer.start_link(
      __MODULE__,
      args,
      name: Module.concat(__MODULE__, topic)
    )
  end

  def start_consumer(args) do
    topic = Keyword.get(args, :topic, "reg_gh:$all")

    case start_link(args) do
      {:ok, pid} ->
        IO.puts("#{Themes.consumer(pid)} for [#{inspect(topic)}] is UP!")

      {:error, {:already_started, pid}} ->
        IO.puts("#{Themes.consumer(pid)} for [#{inspect(topic)}] is UP!")

      {:error, reason} ->
        raise "Failed to start consumer for [#{inspect(topic)}]. 
               Reason: #{inspect(reason)}"
    end
  end
end
