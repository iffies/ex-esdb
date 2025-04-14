defmodule ExESDB.Commanded.Adapter.ChildSpecTest do
  use ExUnit.Case

  alias ExESDB.Commanded.Adapter

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
