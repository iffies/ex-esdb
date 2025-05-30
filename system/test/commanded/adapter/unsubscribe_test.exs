defmodule ExESDB.Commanded.Adapter.UnsubscribeTest do
  @moduledoc false
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

  doctest ExESDB.Commanded.Adapter

  describe "unsubscribe/2" do
    @tag :ex_esdb_commanded
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
