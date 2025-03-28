defmodule ExESDB.TestCase do
  @moduledoc false
  use ExUnit.CaseTemplate

  alias ExESDB.System, as: ESDBSystem
  alias ExESDB.Options, as: Options

  require Logger

  setup do
    {:ok, esdb_meta} = start_supervised({ESDBSystem, Options.fetch!()})
    Logger.debug("esdb_meta: #{inspect(esdb_meta, pretty: true)}")
    [esdb_meta: esdb_meta]
  end

end
