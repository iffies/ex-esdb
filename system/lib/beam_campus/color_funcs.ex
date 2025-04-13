defmodule BeamCampus.ColorFuncs do
  @moduledoc """
    This module is used to manipulate colors.
    It offers a set of functions that can be used to 
    change the color of text in the terminal, using ANSI escape codes.
    It covers all color combinations and effects supported by ANSI.
  """

  # Reset
  def reset, do: "\e[0m"

  # Basic colors (0-7) and bright colors (8-15)
  @colors [
    black: 0,
    red: 1,
    green: 2,
    yellow: 3,
    blue: 4,
    magenta: 5,
    cyan: 6,
    white: 7,
    bright_black: 8,
    bright_red: 9,
    bright_green: 10,
    bright_yellow: 11,
    bright_blue: 12,
    bright_magenta: 13,
    bright_cyan: 14,
    bright_white: 15
  ]

  # Text effects
  @effects [
    bold: 1,
    dim: 2,
    italic: 3,
    underline: 4,
    blink: 5,
    rapid_blink: 6,
    reverse: 7,
    hidden: 8,
    strikethrough: 9
  ]

  # Convert lists to maps for easy lookup
  @colors_map Enum.into(@colors, %{})
  @effects_map Enum.into(@effects, %{})

  def tui(fg_color, bg_color, effects \\ []) do
    fg_code = Map.get(@colors_map, fg_color)
    bg_code = Map.get(@colors_map, bg_color)

    effect_codes =
      effects
      |> Enum.map_join(";", &Map.get(@effects_map, &1))

    #    "\e[38;5;#{fg_code};48;5;#{bg_code + 40};#{effect_codes}m"

    "\e[38;5;#{fg_code};48;5;#{bg_code};#{effect_codes}m"
  end

  # Generate all color combinations
  for {fg_name, fg_code} <- @colors, {bg_name, bg_code} <- @colors do
    fg_code_str = Integer.to_string(fg_code)
    # Background color codes start at 40
    bg_code_str = Integer.to_string(bg_code + 40)
    color_func_name = :"#{fg_name}_on_#{bg_name}"
    color_func_body = "\e[38;5;#{fg_code_str};48;5;#{bg_code_str}m"

    IO.puts("Defining function: #{color_func_name}")

    color_func =
      quote bind_quoted: [f_name: color_func_name, f_body: color_func_body] do
        def unquote(f_name) do
          unquote(f_body)
        end
      end

    quote do
      unquote(color_func)
    end
  end
end
