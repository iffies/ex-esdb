defmodule ExESDB.Repl.EventStreamMonitor do
  @moduledoc false
  use GenServer
  require Logger


  alias ExESDB.Themes, as: Themes
  alias Phoenix.PubSub, as: PubSub

  def start do
    opts = ExESDB.Options.app_env()
    case start_link(opts) do
      {:ok, pid} ->
        Logger.info("Monitor started with pid #{inspect(pid)}")
        pid
      {:error, {:already_started, pid}} ->
        IO.puts "Monitor already started with pid #{inspect(pid)}"
        pid
      {:error, reason} -> raise "Failed to start monitor. Reason: #{inspect(reason)}"
    end
  end

  defp subscribe(opts) do
    store_id = inspect(opts[:store_id])
    case opts[:pub_sub]
    |> PubSub.subscribe(store_id) do
      :ok -> Logger.info("#{Themes.monitor(self())} => Subscribed to #{inspect(opts[:store_id], pretty: true)}")
      error -> Logger.error("#{Themes.monitor(self())} => Failed to subscribe to #{inspect(opts[:store_id], pretty: true)}. Reason: #{inspect(error)}")
    end
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
  def init(opts) do
    Logger.info("#{Themes.monitor(self())} => Starting monitor for #{inspect(opts[:store_id], pretty: true)}")
    subscribe(opts)

    {:ok, opts}
  end


  def start_link(args) do
    GenServer.start_link(
      __MODULE__, 
      args, 
      name: __MODULE__
    )
  end

  def child_spec(opts) do
   %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 5000,
      type: :worker,
    }
  end

end
