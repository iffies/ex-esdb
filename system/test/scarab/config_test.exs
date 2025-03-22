defmodule Scarab.ConfigTest do
  use ExUnit.Case
  doctest Scarab.Config
  alias Scarab.Config

  @app :scarab_es

  test "data_dir/1 returns the data directory" do
    scarab_config = Application.fetch_env!(:scarab_es, :khepri)
    assert Config.data_dir(scarab_config) == "tmp/reg_gh"
  end

  test "store_id/1 returns the store id" do
    scarab_config = Application.fetch_env!(:scarab_es, :khepri)
    assert Config.store_id(scarab_config) == :reg_gh
  end

  test "timeout/1 returns the timeout" do
    scarab_config = Application.fetch_env!(:scarab_es, :khepri)
    assert Config.timeout(scarab_config) == 10_000
  end

  test "db_type/1 returns the database type" do
    scarab_config = Application.fetch_env!(:scarab_es, :khepri)
    assert Config.db_type(scarab_config) == :node
  end

  test "fetch_env!/1 returns the config" do
    config = Config.fetch_env!(:scarab_es)

    assert config.data_dir == "tmp/reg_gh"
  end
end
