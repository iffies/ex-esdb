defmodule BeamCampus.BitFlags.UnsetTest do
  @moduledoc """
    This module contains tests for the BeamCampus.BitFlags module.
  """
  use ExUnit.Case

  alias BeamCampus.BitFlags, as: BitFlags

  describe "unset/2" do
    @tag :bc_bitflags
    test "GIVEN: original_state is `0b00100100` (integer: 36)
          WHEN the flag to be unset is `0b01000000` (integer: 64)
          THEN the result is `0b00010100` (integer: 36)" do
      assert BitFlags.unset(36, 64) == 36
    end
  end
end
