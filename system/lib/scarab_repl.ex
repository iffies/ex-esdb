defmodule ScarabRepl do
  @moduledoc """
  This module is used to start a REPL with the Scarab system running.
  """
  alias Scarab.EventStore, as: ScarabEventStore
  alias Scarab.Config, as: ScarabConfig

  require Logger

  def start() do
    config = ScarabConfig.fetch_env!(:node_app)
    Logger.warning("Starting Scarab REPL")
    Logger.error("Starting Scarab REPL")
    Logger.debug("Starting Scarab REPL")
    Logger.info("Starting Scarab REPL")
    ScarabEventStore.start_link(config)
  end
end
