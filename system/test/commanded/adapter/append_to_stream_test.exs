defmodule ExESDB.Commanded.AppendToStreamTest do
  @moduledoc false
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

  describe "append_to_stream/5" do
    @tag :ex_esdb_commanded
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      stream_uuid = "some-stream"
      expected_version = 1
      events = [%{event_id: "123", event_type: "SomeEvent", data: %{}}]
      opts = []

      # Act
      result = Adapter.append_to_stream(adapter_meta, stream_uuid, expected_version, events, opts)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "append_to_stream/5 is not implemented for " <> _}
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
