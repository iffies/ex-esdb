defmodule ScarabES.SystemTest do
  use ExUnit.Case
  doctest ScarabES.System

  require Logger

  alias ScarabES.System, as: ScarabSystem
  alias ScarabES.EventStore, as: ScarabEventStore
  alias ScarabES.Config, as: ScarabConfig

  @tag :skip
  test "that the Scarab System starts the EventStore in single node mode" do
    config = ScarabConfig.fetch_env!(:node_app)
    res = ScarabSystem.start(config)
    Logger.debug("System pid: #{inspect(res, pretty: true)}")
  end
end
