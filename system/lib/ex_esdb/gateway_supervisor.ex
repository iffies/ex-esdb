defmodule ExESDB.GatewaySupervisor do
  @moduledoc """
    @deprecated "Use ExESDB.GatewaySystem instead"
    
    The GatewaySupervisor is responsible for starting and supervising the
    GatewayWorkers.
    
    This module is deprecated in favor of ExESDB.GatewaySystem which provides
    improved fault tolerance with pooled workers.
  """
  use Supervisor
  require Logger

  @impl Supervisor
  def init(opts) do
    children =
      [
        {ExESDB.GatewayWorker, opts}
      ]

    Logger.info("[#{inspect(self())}][GatewaySupervisor] is UP!", component: :gateway_supervisor, pid: self())
    Supervisor.init(children, strategy: :one_for_one)
  end

  def start_link(opts),
    do:
      Supervisor.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  def child_spec(opts),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :supervisor,
      restart: :permanent,
      shutdown: 5000
    }
end
