defmodule BeamCampus.BitFlags.HasAnyTest do
  @moduledoc """
    This module contains tests for the BeamCampus.BitFlags module.
  """
  use ExUnit.Case

  describe "has_any/2" do
    @tag :bc_bitflags
    test "GIVEN: original_state is `0b00100100` (integer: 36)
            WHEN the flags to be checked are `0b01000000` (integer: 64) and `0b10000000` (integer: 128)
            THEN the result is `true`" do
      assert BitFlags.has_any(36, [64, 128])
    end
  end
end
