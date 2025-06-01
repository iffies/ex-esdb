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
      {ExESDB.System, opts}
    ]

    Logger.warning("#{Themes.app(self())} is UP.")

    opts = [strategy: :one_for_one, name: ExESDB.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def stop(state) do
    ExESDB.System.stop(:normal)
    Logger.warning("STOPPING APP #{inspect(state, pretty: true)}")
  end
end
