defmodule Scarab.ConfigTest do
  use ExUnit.Case
  doctest Scarab.Config
  alias Scarab.Config

  test "data_dir/1 returns the data directory" do
    scarab_config = Application.fetch_env!(:node_app, :scarab)
    assert Config.data_dir(scarab_config) == "tmp/scarab_data"
  end

  test "store_id/1 returns the store id" do
    scarab_config = Application.fetch_env!(:node_app, :scarab)
    assert Config.store_id(scarab_config) == :sell_goods_at_pos
  end

  test "timeout/1 returns the timeout" do
    scarab_config = Application.fetch_env!(:node_app, :scarab)
    assert Config.timeout(scarab_config) == 1_000
  end

  test "db_type/1 returns the database type" do
    scarab_config = Application.fetch_env!(:node_app, :scarab)
    assert Config.db_type(scarab_config) == :node
  end

  test "fetch_env!/1 returns the config" do
    config = Config.fetch_env!(:node_app)

    assert config.data_dir == "tmp/scarab_data"
  end
end
