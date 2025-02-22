defmodule Scarab.ConfigTest do
  use ExUnit.Case
  doctest Scarab.Config
  alias Scarab.Config

  test "data_dir/1 returns the data directory" do
    scarab_config = Application.fetch_env!(:test_app, :scarab)
    assert Config.data_dir(scarab_config) == "tmp/test_dir"
  end

  test "store_id/1 returns the store id" do
    scarab_config = Application.fetch_env!(:test_app, :scarab)
    assert Config.store_id(scarab_config) == :test_store
  end

  test "timeout/1 returns the timeout" do
    scarab_config = Application.fetch_env!(:test_app, :scarab)
    assert Config.timeout(scarab_config) == 1_000
  end
end
