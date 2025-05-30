defmodule ExESDB.Commanded.Adapter.AckEventTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

  doctest ExESDB.Commanded.Adapter

  describe "ack_event/3" do
    test "logs a warning and returns :ok" do
      # Arrange
      meta = %{some: "meta"}
      pid = self()
      event = %{event_id: "123", event_type: "SomeEvent", data: %{}}

      # Act
      result = Adapter.ack_event(meta, pid, event)

      # Assert
      assert result == :ok
      assert_receive {:log, :warning, "ack_event/3 is not implemented for " <> _}
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
