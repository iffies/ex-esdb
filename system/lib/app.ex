defmodule ExESDB.App do
  @moduledoc """
  This module is used to start the Scarab system.
  """
  use Application,
    otp_app: :ex_esdb

    alias ExESDB.Options, as: Options

  require Logger
  require Phoenix.PubSub

  @impl true
  def start(_type, _args) do
    opts = Options.esdb_khepri()

    children = [
      {Phoenix.PubSub, name: :scarab_pubsub},
      {ExESDB.System, opts},
    ]

    :os.set_signal(:sigterm, :handle)

    Logger.info("
                 ===================================================
                 || Starting ExESDB on Node #{inspect(node(), pretty: true)} 
                 ===================================================
      ")

    opts = [strategy: :one_for_one, name: EsESD.Supervisor]
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
