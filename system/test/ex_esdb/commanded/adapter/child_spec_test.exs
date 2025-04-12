defmodule ExESDB.Commanded.Adapter.ChildSpecTest do

  @moduledoc """
    This module contains tests for the child_spec/2 function in the ExESDB.Commanded.Adapter module.
    It is used to test the functionality of the child_spec function in the Commanded.EventStore.Adapter behaviour.
  """
  use ExUnit.Case, async: true
  alias ExESDB.Commanded.Adapter
  alias ExESDB.Options, as: Options
  alias ExESDB.System, as: System

  @test_app :test_app

  describe "ExESDB.Commanded.Adapter.child_spec/2 test" do
    test "returns correct child spec and adapter meta with default configuration" do
      application = @test_app
    end
  end

end
