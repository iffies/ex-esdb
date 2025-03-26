defmodule ExESDB.SystemTest do
  use ExUnit.Case
  doctest ExESDB.System

  require Logger

  alias ExESDB.System, as: ESDBSystem
  alias ExESDB.EventStore, as: EventStore
  alias ExESDB.Options, as: Opttions

  @tag :skip
  test "that the Scarab System starts the EventStore in single node mode" do
    opts = Options.esdb_khepri()
    res = ScarabSystem.start(opts)
    Logger.debug("System pid: #{inspect(res, pretty: true)}")
  end
end
