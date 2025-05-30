defmodule ExESDB.Commanded.Adapter.RecordSnapshotTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

  describe "record_snapshot/2" do
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      snapshot_data = %{some: "data"}

      # Act
      result = Adapter.record_snapshot(adapter_meta, snapshot_data)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "record_snapshot/3 is not implemented for " <> _}
    end
  end

  # Helper function to capture log messages
  setup do
    :meck.new(Logger, [:passthrough])

    :meck.expect(Logger, :warning, fn message ->
      send(self(), {:log, :warning, message})
      :ok
    end)

    on_exit(fn ->
      :meck.unload(Logger)
    end)
  end
end
