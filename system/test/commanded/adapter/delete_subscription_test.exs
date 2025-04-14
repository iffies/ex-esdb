defmodule ExESDB.Commanded.Adapter.DeleteSubscriptionTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

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
