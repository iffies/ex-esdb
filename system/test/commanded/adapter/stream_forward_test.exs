defmodule ExESDB.Commanded.Adapter.StreamForwardTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

  describe "stream_forward/4" do
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      stream_uuid = "some-stream"
      start_version = 0
      read_batch_size = 100

      # Act
      result = Adapter.stream_forward(adapter_meta, stream_uuid, start_version, read_batch_size)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "stream_forward/5 is not implemented for " <> _}
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
