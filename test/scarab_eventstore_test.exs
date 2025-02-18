defmodule Scarab.EventstoreTest do
  use ExUnit.Case

  doctest Scarab.Eventstore

  alias Scarab.Eventstore, as: Eventstore
  alias Schema.ScarabInit, as: ScarabInit

  test "starts a default eventstore" do
    pid = Eventstore.start([])
    assert pid != nil
    assert Process.alive?(pid)
    assert File.exists?("khepri#nonode@nohost/names.dets")
  end

  test "starts a custom eventstore" do
    pid = Eventstore.start(config: %ScarabInit{data_dir: "tmp", store_id: "test"})
    assert pid != nil
    assert Process.alive?(pid)
    assert File.exists?("tmp/names.dets")
  end
end
