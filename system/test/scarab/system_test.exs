defmodule Scarab.SystemTest do
  use ExUnit.Case
  doctest Scarab.System

  require Logger

  alias Scarab.System, as: ScarabSystem
  alias Scarab.EventStore, as: ScarabEventStore
  alias Scarab.Config, as: ScarabConfig

  @tag :skip
  test "that the Scarab System starts the EventStore in single node mode" do
    config = ScarabConfig.fetch_env!(:node_app)
    res = ScarabSystem.start(config)
    Logger.debug("System pid: #{inspect(res, pretty: true)}")
  end
end
