
defmodule ExESDB.OptionsTest do
  use ExUnit.Case, async: true
  @doctest ExESDB.Options
  
  import ExESDB.Options
  alias ExESDB.EnVars, as: EnVars

  describe "opts/1" do
    setup do
      original = Application.get_env(:ex_esdb, :khepri)
      Application.put_env(:ex_esdb, :khepri, %{test_key: "test_value"})
      on_exit(fn -> Application.put_env(:ex_esdb, :khepri, original) end)
    end

    test "returns config value for given key" do
      assert app_env(:test_key) == "test_value"
    end
  end

  describe "data_dir/0" do
    test "returns env var when set" do
      System.put_env(EnVars.data_dir(), "/custom_data")
      assert data_dir() == "/custom_data"
    end

    test "returns config value when env not set" do
      Application.put_env(:ex_esdb, :khepri, %{data_dir: "/config_data"})
      assert data_dir() == "/config_data"
    end

    test "returns default when neither set" do
      System.delete_env(EnVars.data_dir())
      Application.put_env(:ex_esdb, :khepri, %{})
      assert data_dir() == "/data"
    end
  end

  describe "store_id/0" do
    test "converts env string to atom" do
      System.put_env(EnVars.store_id(), "custom_store")
      assert store_id() == :custom_store
    end

    test "returns config atom when env not set" do
      Application.put_env(:ex_esdb, :khepri, %{store_id: :config_store})
      assert store_id() == :config_store
    end

    test "returns default atom when neither set" do
      assert store_id() == :ex_store
    end
  end

  describe "timeout/0" do
    test "converts env string to integer" do
      System.put_env(EnVars.timeout(), "5000")
      assert timeout() == 5000
    end

    test "returns config value when env not set" do
      Application.put_env(:ex_esdb, :khepri, %{timeout: 7500})
      assert timeout() == 7500
    end

    test "returns default when neither set" do
      assert timeout() == 10_000
    end
  end

  describe "seed_nodes/0" do
    test "parses env string into atoms" do
      System.put_env(EnVars.seed_nodes(), "node1, node-2, node three")
      assert seed_nodes() == [:node1, :node_2, :node_three]
    end

    test "returns config list when env not set" do
      Application.put_env(:ex_esdb, :khepri, %{seeds_nodes: [:n1, :n2]})
      assert seed_nodes() == [:n1, :n2]
    end

    test "returns default list when neither set" do
      assert seed_nodes() == [node()]
    end
  end

end
