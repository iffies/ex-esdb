defmodule ExESDB.App do
  @moduledoc """
  This module is used to start the ExESDB system.
  """
  use Application,
    otp_app: :ex_esdb

  alias ExESDB.Options, as: Options
  alias ExESDB.Themes, as: Themes

  require Logger
  require Phoenix.PubSub

  @impl true
  def start(_type, _args) do
    opts = Options.app_env()

    children = [
      {ExESDB.System, opts},
    ]

    :os.set_signal(:sigterm, :handle)

    Logger.warning("#{Themes.app(self())} is UP.")

    opts = [strategy: :one_for_one, name: EsESDB.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def handle_info(:sigterm, _state) do
    Logger.info("SIGTERM received. Stopping ExESDB")
    Application.stop(:ex_esdb)
  end

  @impl true
  def stop(state) do
    Logger.info("STOPPING APP #{inspect(state, pretty: true)}")
  end

end
