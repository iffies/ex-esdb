defmodule BeamCampus.ColorFuncsTest do
  @moduledoc """
    This module contains tests for the BeamCampus.ColorFuncs module.
  """
  use ExUnit.Case
  @tag :bc_color_funcs
  @tag :bc_docs
  doctest BeamCampus.ColorFuncs

  describe "color_0_on_0" do
    @tag :bc_color_funcs
    test "GIVEN we start with a black background and a white foreground
          WHEN we call color_0_on_0
          THEN the result is a black foreground on a white background" do
      assert BeamCampus.ColorFuncs.color_0_on_0() == "\e[38;5;0;48;5;0m"
    end
  end
end
