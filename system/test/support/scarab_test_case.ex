defmodule ScarabTestCase do
  @moduledoc false
  use ExUnit.CaseTemplate

  alias Scarab.System, as: ScarabSystem
  alias Scarab.Config, as: ScarabConfig

  require Logger

  setup do
    {:ok, scarab_meta} = start_scarab()
    Logger.debug("scarab_meta: #{inspect(scarab_meta, pretty: true)}")
    [scarab_meta: scarab_meta]
  end

  def start_scarab(config \\ []) do
    config = ScarabConfig.fetch_env!(:node_app)
    start_supervised!({ScarabSystem, config})
  end
end
