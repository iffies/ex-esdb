defmodule ExESDB.Commanded.Adapter.ReadSnapshotTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

  describe "read_snapshot/2" do
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      stream_uuid = "some-stream"

      # Act
      result = Adapter.read_snapshot(adapter_meta, stream_uuid)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "read_snapshot/5 is not implemented for " <> _}
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
