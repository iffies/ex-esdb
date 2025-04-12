defmodule ExESDB.Commanded.AdapterTest do
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

  describe "append_to_stream/5" do
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

  describe "child_spec/2" do
    test "logs a warning and returns {:ok, [nil]}" do
      # Arrange
      application = :some_app
      opts = []

      # Act
      result = Adapter.child_spec(application, opts)

      # Assert
      assert result == {:ok, [nil]}
      assert_receive {:log, :warning, "child_spec/2 is not implemented for " <> _}
    end
  end

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

  describe "delete_subscription/3" do
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      arg2 = "some-arg"
      subscription_name = "some-subscription"

      # Act
      result = Adapter.delete_subscription(adapter_meta, arg2, subscription_name)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "delete_subscription/4 is not implemented for " <> _}
    end
  end

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

  describe "subscribe/2" do
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      arg2 = "some-arg"

      # Act
      result = Adapter.subscribe(adapter_meta, arg2)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "subscribe/2 is not implemented for " <> _}
    end
  end

  describe "subscribe_to/6" do
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      arg2 = "some-arg"
      subscription_name = "some-subscription"
      subscriber = self()
      start_from = :origin
      opts = []

      # Act
      result = Adapter.subscribe_to(adapter_meta, arg2, subscription_name, subscriber, start_from, opts)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "subscribe_to/7 is not implemented for " <> _}
    end
  end

  describe "unsubscribe/2" do
    test "logs a warning and returns {:error, :not_implemented}" do
      # Arrange
      adapter_meta = %{some: "meta"}
      subscription_name = "some-subscription"

      # Act
      result = Adapter.unsubscribe(adapter_meta, subscription_name)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "unsubscribe/3 is not implemented for " <> _}
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
