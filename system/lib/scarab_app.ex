defmodule ScarabApp do
  @moduledoc """
  This module is used to start the Scarab system.
  """
  use Application, otp_app: :scarab_app

  @impl true
  def start(_type, _args) do
    config = Scarab.Config.fetch_env!(:scarab_app)

    children = [
      {Scarab.System, config, name: Scarab.System}
    ]

    opts = [strategy: :one_for_one, name: ScarabApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
