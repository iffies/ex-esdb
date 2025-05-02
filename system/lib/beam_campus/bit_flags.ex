defmodule BeamCampus.BitFlags do
  @moduledoc """
    This module is used to manipulate bitwise flags.

    Inspired by: [Flags in C#](https://stackoverflow.com/questions/8447/what-does-the-flags-enum-attribute-mean-in-c)

    Event sourced systems often rely on flags to indicate the state of the aggregate at any given time.
    In essence, an event sourced aggregate is a finite state machine and this state is 
    often represented as a set of flags, to be used as a shorthand for the state of the aggregate.

    In this module, we define a set of functions that can be used to manipulate these flags.
  """
  import Bitwise

  @doc """
  ### Description

    Returns the bitwise OR of two flags.
    In other words, it sets the bit that corresopnds to the flag

    - GIVEN: original_state is `0b00100100` (integer: 36) 
    - WHEN the flag to be set is `0b01000000` (integer: 64)
    - THEN the result is `0b01100100` (integer: 100)

  ### Parameters

    - target: the integer to be set
    - flag: the integer to be set
  ### Output
    
    The integer representation of the state after setting the flag

  ### Examples

    iex> BitFlags.set(36, 64)
    100
  """
  def set(target, flag)
      when is_integer(target) and
             is_integer(flag),
      do: target ||| flag

  @doc """
  ### Description

    Returns the bitwise AND of two flags.
    In other words, it unsets the bit that corresopnds to the flag

    - GIVEN: original_state is `0b01100100` (integer: 100)
    - WHEN the flag to be unset is `0b01000000` (integer: 64)
    - THEN the result is `0b00100100` (integer: 36)
    
  ### Parameters

      - `target`: the integer representation of the state
      - `flag`  : the integer representation of the flag

  ### Output

    The integer representation of the state after unsetting the flag

  ### Examples    

      iex> BitFlags.unset(100, 64)
      36
  """
  def unset(target, flag)
      when is_integer(target) and
             is_integer(flag),
      do: target &&& bnot(flag)

  @doc """
    Returns the bitwise OR of multiple flags against a given state.
    In other words, it sets the bits that corresopnds to the flags
    GIVEN: original_state is `0b00100100` (integer: 36)
    WHEN the flags to be set are `[0b01000000, 0b10000000]` (integers: 64, 128)
    THEN the result is `0b11100100` (integer: 228)

    Example:
    iex> BitFlags.set_all(36, [64, 128])
    228
  """
  def set_all(target, flags) do
    Enum.reduce(flags, target, fn flag, acc ->
      acc ||| flag
    end)
  end

  @doc """
    Returns the bitwise AND of multiple flags against a given state.
    In other words, it unsets the bits that corresopnds to the flags
    GIVEN: original_state is `0b11100100` (integer: 228)
    WHEN the flags to be unset are `[0b01000000, 0b10000000]` (integers: 64, 128)
    THEN the result is `0b00100100` (integer: 36)

    Example:
    iex> BitFlags.unset_all(228, [64, 128])
    36
  """
  def unset_all(target, flags) do
    Enum.reduce(flags, target, fn flag, acc ->
      acc &&& bnot(flag)
    end)
  end

  @doc """
    Returs true if the given flag is set in the target state.
    In other words, it returns true if the bit that corresponds to the flag is set.
    GIVEN: original_state is `0b01100100` (integer: 100)
    WHEN the flag to be checked is `0b01000000` (integer: 64)
    THEN the result is `true`

    Example:
    iex> BitFlags.has?(100, 64)
    true
  """
  def has?(target, flag), do: (target &&& flag) == flag

  @doc """
    Returns true if the given flag is NOT set in the target state.
    In other words, it returns true if the bit that corresponds to the flag is NOT set.
    GIVEN: original_state is `0b01100100` (integer: 100)
    WHEN the flag to be checked is `0b01000000` (integer: 64)
    THEN the result is `false`
    AND WHEN the flag to be checked is `0b00000100` (integer: 8)
    THEN the result is `true`

    Example:
    iex> BitFlags.has_not?(100, 64)
    false
    iex> BitFlags.has_not?(100, 8)
    true
  """
  def has_not?(target, flag), do: (target &&& flag) != flag

  @doc """
    Returns true if ALL the flags are set in the target state.
    In other words, it returns true if ALL the bits that correspond to the flags are set.
    GIVEN: original_state is `0b01100100` (integer: 100)
    WHEN the flags to be checked are `[0b01000000, 0b10000000]` (integers: 64, 128)
    THEN the result is `true`
    AND WHEN the flags to be checked are `[0b01000000, 0b00000100]` (integers: 64, 8)
    THEN the result is `false`

    Example:
    iex> BitFlags.has_all?(100, [64, 128])
    true
    iex> BitFlags.has_all?(100, [64, 8])
    false
  """
  def has_all?(status, flags) do
    flags |> Enum.all?(fn flag -> has?(status, flag) end)
  end

  @doc """
    Returns true if any of the flags are set in the target state.
    In other words, it returns true if any of the bits that correspond to the flags are set.
    GIVEN: original_state is `0b01100100` (integer: 100)
    WHEN the flags to be checked are `[0b01000000, 0b10000000]` (integers: 64, 128)
    THEN the result is `true`

    Example:
    iex> BitFlags.has_any?(100, [64, 128])
    true
  """
  def has_any?(status, flags) do
    flags |> Enum.any?(fn flag -> has?(status, flag) end)
  end

  @doc """
    Returns a list of flag descriptions that are set in the target state.
    GIVEN: original_state is `0b01100100` (integer: 100)
    AND the flag_map is:
       %{
          0 => "None",
          1 => "Ready",
          2 => "In Progress",
          4 => "Completed",
          8 => "Cancelled",
          16 => "Failed",
          32 => "Archived",
          64 => "Ready to Archive",
          128 => "Ready to Publish",
          256 => "Published",
          512 => "Unpublished",
       }
    WHEN the target state is `0b01100100` (integer: 100)
    THEN the result is `["Completed", "Archived", "Ready to Archive"]`

    Example: 
    iex> descriptions = 
    ...>  %{
    ...>       0 => "None",
    ...>       1 => "Ready",
    ...>       2 => "In Progress",
    ...>       4 => "Completed",
    ...>       8 => "Cancelled",
    ...>       16 => "Failed",
    ...>       32 => "Archived",
    ...>       64 => "Ready to Archive",
    ...>       128 => "Ready to Publish",
    ...>       256 => "Published",
    ...>       512 => "Unpublished",
    ...>  }
    iex> BitFlags.to_list(100, descriptions)
    ["Completed", "Archived", "Ready to Archive"]
  """
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

  @doc """
    Returns the highest flag in the target state.
    GIVEN: target_state is `0b01100100` (integer: 100)    
    AND the flag_map is:
        %{
           0 => "None",
           1 => "Ready",
           2 => "In Progress",
           4 => "Completed",
           8 => "Cancelled",
           16 => "Failed",
           32 => "Archived",
           64 => "Ready to Archive",
           128 => "Ready to Publish",
        }
    THEN the result is `Ready to Archive`

    Example:
    iex> descriptions = 
    ...>  %{
    ...>       0 => "None",
    ...>       1 => "Ready",
    ...>       2 => "In Progress",
    ...>       4 => "Completed",
    ...>       8 => "Cancelled",
    ...>       16 => "Failed",
    ...>       32 => "Archived",
    ...>       64 => "Ready to Archive",
    ...>       128 => "Ready to Publish",
    ...>  }
    %{
       0 => "None",
       1 => "Ready",
       2 => "In Progress",
       4 => "Completed",
       8 => "Cancelled",
       16 => "Failed",
       32 => "Archived",
       64 => "Ready to Archive",
       128 => "Ready to Publish",
    }
    iex> BitFlags.highest(100, descriptions)
    "Ready to Archive"
  """
  def highest(n, flag_map) do
    [head | _] =
      to_list(n, flag_map)
      |> Enum.reverse()

    head
  end

  @doc """
    Returns the lowest flag in the bit flag map.
    GIVEN: target_state is `0b01100100` (integer: 100)
    AND the flag_map is:
        %{
           0 => "None",
           1 => "Ready",
           2 => "In Progress",
           4 => "Completed",
           8 => "Cancelled",
           16 => "Failed",
           32 => "Archived",
           64 => "Ready to Archive",
           128 => "Ready to Publish",
        }
     THEN the result is `Ready`

    Example:
    iex> descriptions = 
    ...>  %{
    ...>       0 => "None",
    ...>       1 => "Ready",
    ...>       2 => "In Progress",
    ...>       4 => "Completed",
    ...>       8 => "Cancelled",
    ...>       16 => "Failed",
    ...>       32 => "Archived",
    ...>       64 => "Ready to Archive",
    ...>       128 => "Ready to Publish",
    ...>  }
    %{
       0 => "None",
       1 => "Ready",
       2 => "In Progress",
       4 => "Completed",
       8 => "Cancelled",
       16 => "Failed",
       32 => "Archived",
       64 => "Ready to Archive",
       128 => "Ready to Publish",
    }
    iex> BitFlags.lowest(100, descriptions)
    "Ready" 
  """
  def lowest(n, flag_map) do
    [head | _] =
      to_list(n, flag_map)

    head
  end

  @doc """
    Returns a string representation of the bit flags.
    GIVEN: target_state is `0b01100100` (integer: 100)
    AND the flag_map is:
         %{
           0 => "None",
           1 => "Ready",
           2 => "In Progress",
           4 => "Completed",
           8 => "Cancelled",
           16 => "Failed",
           32 => "Archived",
           64 => "Ready to Archive",
           128 => "Ready to Publish",
        }
    THEN the result is `"Completed, Archived, Ready to Archive"`

    Example:
    iex> descriptions =
    ...>  %{
    ...>       0 => "None",
    ...>       1 => "Ready",
    ...>       2 => "In Progress",
    ...>       4 => "Completed",
    ...>       8 => "Cancelled",
    ...>       16 => "Failed",
    ...>       32 => "Archived",
    ...>       64 => "Ready to Archive",
    ...>       128 => "Ready to Publish",
    ...>  }
    iex> BitFlags.to_string(100, descriptions)
    "Completed, Archived, Ready to Archive"
  """
  def to_string(n, flag_map) do
    to_list(n, flag_map)
    |> Enum.join(", ")
  end

  defp decompose(0, _, acc), do: Enum.reverse(acc)

  defp decompose(target, power, acc) do
    if Bitwise.band(target, power) != 0 do
      decompose(target - power, power <<< 1, [power | acc])
    else
      decompose(target, power <<< 1, acc)
    end
  end

  def decompose(target) when target > 0 do
    decompose(target, 1, [])
  end
end
