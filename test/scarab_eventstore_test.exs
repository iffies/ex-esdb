defmodule Scarab.EventstoreTest do
  use ExUnit.Case

  doctest Scarab.Eventstore

  alias Scarab.Eventstore, as: Eventstore
  alias Schema.ScarabInit, as: ScarabInit

  @tag :skip
  test "starts a default eventstore" do
    pid = Eventstore.start([])
    assert pid != nil
    assert Process.alive?(pid)
    assert File.exists?("khepri#nonode@nohost/names.dets")
  end

  test "starts a custom eventstore" do
    pid =
      Eventstore.start(
        config: %ScarabInit{
          data_dir: "tmp/from_code",
          store_id: :test_store0,
          timeout: 1_000
        }
      )

    assert pid != nil
    assert Process.alive?(pid)
    assert File.dir?("tmp/from_code")
  end
end
