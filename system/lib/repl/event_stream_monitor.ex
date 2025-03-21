defmodule Scarab.Repl.EventStreamMonitor do
  @moduledoc false
  use GenServer
  require Logger

  alias Phoenix.PubSub, as: PubSub

  defp subscribe(store) do
    :scarab_pubsub 
      |> PubSub.subscribe("#{store}")
    end
 
  @impl true
  def handle_info({:event_seen, event}, state) do
    IO.puts "Seen event #{inspect event}"
    {:noreply, state}
  end

  @impl true
  def handle_info(unknown, state) do
    IO.puts "Unknown message #{inspect unknown}"
    {:noreply, state}
  end


  @impl true
  def init(%{store_id: store} = args) do
    Logger.info("#{Colors.store_theme(self())} => Starting monitor for #{inspect(store, pretty: true)}")
    store 
    |> subscribe
    {:ok, args}
  end


  def start_link(args) do
    GenServer.start_link(
      __MODULE__, 
      args, 
      name: __MODULE__
    )
  end

  def child_spec(args) do
   %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :permanent,
      shutdown: 5000,
      type: :worker,
    }
  end

end
