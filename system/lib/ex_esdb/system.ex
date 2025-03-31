defmodule ExESDB.System do
  @moduledoc """
    This module is the top level supervisor for the ExESDB system.
    It is responsible for supervising:
    - the event store
    - the cluster
    - the Event Enitter (pub/sub)
  """
  use Supervisor

  alias ExESDB.Themes, as: Themes

  require Logger
  require Phoenix.PubSub

  @impl true
  def init(opts) do
    Logger.info("#{Themes.system(self())} is UP.")

    children = [
      {Phoenix.PubSub, name: opts[:pub_sub]},
      {ExESDB.EventStore, opts},
      {ExESDB.Cluster, opts},
      {ExESDB.EventProjector, opts}
    ]

    Supervisor.init(
      children,
      strategy: :one_for_one
    )
  end

  # def handle_info({:EXIT, pid, reason}, state) do
  #   Logger.warning("ExESDB.trapped EXIT from [#{inspect(pid)}]
  #     with reason [#{inspect(reason, pretty: true)}]")
  #   Supervisor.terminate_child(__MODULE__, pid)
  #   Supervisor.restart_child(__MODULE__, pid)
  #   {:noreply, state}
  # end

  def start_link(opts),
    do:
      Supervisor.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  # def start(opts) do
  #   case Supervisor.start_link(
  #          __MODULE__,
  #          opts,
  #          name: __MODULE__
  #        ) do
  #     {:ok, pid} -> pid
  #     {:error, {:already_started, pid}} -> pid
  #     {:error, reason} -> raise "failed to start eventstores supervisor: #{inspect(reason)}"
  #   end
  # end

  #
  # def start_link(opts) do
  #   Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  # end
  #
  #
  # def child_spec(opts) do
  #   %{
  #     id: __MODULE__,
  #     start: {__MODULE__, :start, [opts]},
  #     type: :supervisor
  #   }
  # end
end
