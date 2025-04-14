defmodule BeamCampus.ColorFuncsTest do
  @moduledoc """
    This module contains tests for the BeamCampus.ColorFuncs module.
  """
  use ExUnit.Case
  @tag :bc_color_funcs
  @tag :bc_docs
  doctest BeamCampus.ColorFuncs

  describe "black_on_white/0" do
    @tag :bc_color_funcs
    test "GIVEN we start with a black background and a white foreground
          WHEN we call color_0_on_0
          THEN the result is a black foreground on a white background" do
      assert BeamCampus.ColorFuncs.black_on_white() == "\e[30m\e[47m"
    end

    @tag :bc_color_funcs
    test "GIVEN we start with a black background and a white foreground
          WHEN we call red_on_black()
          THEN the result is a red foreground on a black background" do
      assert BeamCampus.ColorFuncs.red_on_black() == "\e[31m\e[40m"
    end
  end
end
