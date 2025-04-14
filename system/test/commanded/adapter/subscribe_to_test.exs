defmodule ExESDB.Commanded.Adapter.SubscribeToTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

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
      result =
        Adapter.subscribe_to(adapter_meta, arg2, subscription_name, subscriber, start_from, opts)

      # Assert
      assert result == {:error, :not_implemented}
      assert_receive {:log, :warning, "subscribe_to/7 is not implemented for " <> _}
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
