defmodule Scarab.VersionFormatter do
  @moduledoc false

  ## Examples
  # iex > version_to_integer("0042")
  # 42

  # iex > version_to_integer("123")
  # 123

  # iex > version_to_integer("000")
  # 0

  def version_to_integer(padded_version) when is_binary(padded_version) do
    padded_version
    |> String.trim_leading("0")
    |> case do
      # Handle all-zero case
      "" -> 0
      num_str -> String.to_integer(num_str)
    end
  end

  # Original padding function remains unchanged
  def pad_version(version, length) when is_integer(version) and length > 0 do
    version
    |> Integer.to_string()
    |> String.pad_leading(length, "0")
  end
end
