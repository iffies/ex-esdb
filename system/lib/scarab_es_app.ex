defmodule Scarab.ESApp do
  @moduledoc """
  This module is used to start the Scarab system.
  """
  use Application,
    otp_app: :scarab_es

  require Logger
  require Phoenix.PubSub

  @impl true
  def start(_type, _args) do
    config = Scarab.Config.fetch_env!(:scarab_es)

    children = [
      {Phoenix.PubSub, name: :scarab_pubsub},
      {Scarab.System, config},
    ]

    Logger.info("
                 ===================================================
                 || Starting Scarab Eventstore on #{inspect(node(), pretty: true)} ||
                 ===================================================
      ")

    opts = [strategy: :one_for_one, name: Scarab.ESApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
