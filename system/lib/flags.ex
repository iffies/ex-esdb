defmodule Flags do
  @moduledoc """
    This module is used to manipulate bitwise flags.

    Inspired by: [Flags in C#](https://stackoverflow.com/questions/8447/what-does-the-flags-enum-attribute-mean-in-c)
    
    Event souurced systems often rely on flags to indicate the state of the aggregate at any given time.
    In this module, we define a set of functions that can be used to manipulate these flags.
  """
  import Bitwise

  @doc """
    Returns the bitwise OR of two flags.
    In other words, it sets the bit that corresopnds to the flag
    GIVEN: original_state is `0b00100100` (integer: 36) 
    WHEN the flag to be set is `0b01000000` (integer: 64)
    THEN the result is `0b01100100` (integer: 100)

    Example:
    iex> Flags.set(36, 64)
    100  
  """
  def set(target, flag) 
    when is_integer(target) 
      and is_integer(flag), 
    do: target ||| flag
  @doc """
    Returns the bitwise AND of two flags.
    In other words, it unsets the bit that corresopnds to the flag
    GIVEN: original_state is `0b01100100` (integer: 100)
    WHEN the flag to be unset is `0b01000000` (integer: 64)
    THEN the result is `0b00100100` (integer: 36)

    Example:
    iex> Flags.unset(100, 64)
    36
  """ 
  def unset(target, flag) 
    when is_integer(target) 
      and is_integer(flag), 
    do: target &&& bnot(flag)
  @doc """
    Returns the bitwise OR of multiple flags against a given state.
    In other words, it sets the bits that corresopnds to the flags
    GIVEN: original_state is `0b00100100` (integer: 36)
    WHEN the flags to be set are `[0b01000000, 0b10000000]` (integers: 64, 128)
    THEN the result is `0b11100100` (integer: 228)

    Example:
    iex> Flags.set_all(36, [64, 128])
    228
  """
  def set_all(target, flags) do
    Enum.reduce(flags, target, fn flag, acc ->
      acc ||| flag
    end)
  end

  def unset_all(target, flags) do
    Enum.reduce(flags, target, fn flag, acc ->
      acc &&& bnot(flag)
    end)
  end

  def has?(target, flag), do: (target &&& flag) == flag
  def has_not?(target, flag), do: (target &&& flag) != flag

  def to_list(0, flag_map), do: [flag_map[0]]
  def to_list(n, flag_map) when n > 0 do
    # Extract keys (powers of 2) from the map and sort them
    keys = Map.keys(flag_map) |> Enum.sort()
    # Iterate through the keys and collect the flags that are set in `n`
    flags =
      Enum.reduce(keys, [], fn key, acc ->
        if Bitwise.band(n, key) != 0 do
          [flag_map[key] | acc]
        else
          acc
        end
      end)
    # Since we collected the flags in reverse order, reverse the list before returning
    Enum.reverse(flags)
  end

  def highest(n, flag_map) do
    [head | _] =
      to_list(n, flag_map)
      |> Enum.reverse()

    head
  end

  def has_all?(status, flags) do
    flags |> Enum.all?(fn flag -> has?(status, flag) end)
  end

  def has_any?(status, flags) do
    flags |> Enum.any?(fn flag -> has?(status, flag) end)
  end

  def lowest(n, flag_map) do
    [head | _] =
      to_list(n, flag_map)

    head
  end

  def to_string(n, flag_map) do
    to_list(n, flag_map)
    |> Enum.join(", ")
  end

  def decompose(target) when target > 0 do
    decompose(target, 1, [])
  end

  defp decompose(0, _, acc), do: Enum.reverse(acc)
  defp decompose(target, power, acc) do
    if Bitwise.band(target, power) != 0 do
      decompose(target - power, power <<< 1, [power | acc])
    else
      decompose(target, power <<< 1, acc)
    end
  end


end
