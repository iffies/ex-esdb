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
    topic = topic(args)
    pubsub = pubsub(args)

    pubsub
    |> Phoenix.PubSub.subscribe(topic)

    {:ok, args}
  end

  def start_link(args) do
    topic = topic(args)

    GenServer.start_link(
      __MODULE__,
      args,
      name: Module.concat(__MODULE__, topic)
    )
  end

  @spec start(keyword()) :: pid()
  @doc """
    Starts a consumer process for testing purposes.
    ## Parameters
      * `topic`: The topic to consume events from (string, default: `reg_gh:$all`).   
  """
  def start(args) do
    topic = topic(args)

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

  defp topic(args), do: Keyword.get(args, :topic, "reg_gh:$all")
  defp pubsub(args), do: Keyword.get(args, :pubsub, :ex_esdb_pubsub)
end
