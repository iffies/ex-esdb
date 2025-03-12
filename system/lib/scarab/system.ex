defmodule Scarab.System do
  @moduledoc false
  use Supervisor

  require Logger

  @impl true
  def init(config) do
    children = [
      {Phoenix.PubSub, name: :scarab_pubsub},
      {Scarab.EventStore, config}
    ]

    Supervisor.init(
      children,
      strategy: :one_for_one,
      max_restarts: 10,
      max_seconds: 60
    )
  end

  def handle_info({:EXIT, pid, reason}, state) do
    Logger.warning("Scarab trapped EXIT from [#{inspect(pid)}] 
      with reason [#{inspect(reason, pretty: true)}]")
    Supervisor.terminate_child(__MODULE__, pid)
    Supervisor.restart_child(__MODULE__, pid)
    {:noreply, state}
  end

  def start(config) do
    case Supervisor.start_link(
           __MODULE__,
           config,
           name: __MODULE__
         ) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      {:error, reason} -> raise "failed to start eventstores supervisor: #{inspect(reason)}"
    end
  end
end
