defmodule BitFlagsTest do
  @moduledoc """
    This module contains tests for the BitFlags module.
  """
  use ExUnit.Case

  @tag :bitflags
  doctest BitFlags

  describe "set/2" do
    @tag :bitflags
    test "GIVEN: original_state is `0b00100100` (integer: 36)
          WHEN the flag to be set is `0b01000000` (integer: 64)
          THEN the result is `0b01100100` (integer: 100)" do
      assert BitFlags.set(36, 64) == 100
    end
  end
end
