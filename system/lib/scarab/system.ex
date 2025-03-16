defmodule Scarab.System do
  @moduledoc false
  use Supervisor

  require Phoenix.PubSub

  require Logger

  @impl true
  def init(config) do
    children = [
      {Scarab.EventStore, config}
    ]

    Supervisor.init(
      children,
      strategy: :one_for_one
    )
  end

  def handle_info({:EXIT, pid, reason}, state) do
    Logger.warning("Scarab trapped EXIT from [#{inspect(pid)}] 
      with reason [#{inspect(reason, pretty: true)}]")
    Supervisor.terminate_child(__MODULE__, pid)
    Supervisor.restart_child(__MODULE__, pid)
    {:noreply, state}
  end

  def start_link(config) do
    Supervisor.start_link(__MODULE__, config, name: __MODULE__)
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
  #
  # def start_link(config) do
  #   Supervisor.start_link(__MODULE__, config, name: __MODULE__)
  # end
  #
  #
  # def child_spec(config) do
  #   %{
  #     id: __MODULE__,
  #     start: {__MODULE__, :start, [config]},
  #     type: :supervisor
  #   }
  # end
end
