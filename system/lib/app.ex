defmodule ExESDB.App do
  @moduledoc """
  This module is used to start the ExESDB system.
  """
  use Application,
    otp_app: :ex_esdb

  alias ExESDB.Options, as: Options

  require Logger
  require Phoenix.PubSub

  @impl true
  def start(_type, _args) do
    config = Options.app_env()
    store_id = config[:store_id]
    Logger.warning("Attempting to start ExESDB with options: #{inspect(config, pretty: true)}")

    children = [
      {ExESDB.System, config}
    ]

    opts = [strategy: :one_for_one, name: ExESDB.Supervisor]
    res = Supervisor.start_link(children, opts)

    Logger.info("🚀 ExESDB.App #{inspect(self())} is UP for store #{inspect(store_id)}", component: :app)

    res
  end

  @impl true
  def stop(state) do
    ExESDB.System.stop(:normal)
    Logger.warning("STOPPING APP #{inspect(state, pretty: true)}")
  end
end
