defmodule ExESDB.GatewaySupervisor do
  @moduledoc """
    The GatewaySupervisor is responsible for starting and supervising the
    GatewayWorkers.
  """
  use Supervisor
  require Logger
  alias ExESDB.Themes, as: Themes

  @impl Supervisor
  def init(opts) do
    children =
      [
        {ExESDB.GatewayWorker, opts}
      ]

    IO.puts("#{Themes.gateway_supervisor(self())} is UP!")
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
