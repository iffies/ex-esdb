defmodule ExESDB.Commanded.Adapter.DeleteSnapshotTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

  describe "delete_snapshot/2" do
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      source_uuid = "some-source"

      # Act
      result = Adapter.delete_snapshot(adapter_meta, source_uuid)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "delete_snapshot/4 is not implemented for " <> _}
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
