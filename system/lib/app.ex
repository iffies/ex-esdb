defmodule ScarabES.App do
  @moduledoc """
  This module is used to start the Scarab system.
  """
  use Application,
    otp_app: :scarab_es

  require Logger
  require Phoenix.PubSub

  @impl true
  def start(_type, _args) do
    config = ScarabES.Config.fetch_env!()

    children = [
      {Phoenix.PubSub, name: :scarab_pubsub},
      {ScarabES.System, config},
    ]

    :os.set_signal(:sigterm, :handle)

    Logger.info("
                 ===================================================
                 || Starting Scarab on Node #{inspect(node(), pretty: true)} 
                 ===================================================
      ")

    opts = [strategy: :one_for_one, name: Scarab.ESApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def handle_info(:sigterm, _state) do
    Logger.info("SIGTERM received. Stopping Scarab")
    Application.stop(:scarab_es)
  end

  @impl true
  def stop(state) do
    Logger.info("STOPPING APP #{inspect(state, pretty: true)}")
  end


end
