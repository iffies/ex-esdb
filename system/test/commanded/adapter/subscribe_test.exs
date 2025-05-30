defmodule ExESDB.Commanded.Adapter.SubscribeTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

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
