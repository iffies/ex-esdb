defmodule ExESDB.Registry do
  @moduledoc """
    Provides functions for working with event store pub/sub.
  """
  use Horde.Registry

  def start_link(opts) do
    Horde.Registry.start_link(
      __MODULE__,
      [keys: :unique] ++ opts,
      name: __MODULE__
    )
  end

  def init(init_arg) do
    [members: members()]
    |> Keyword.merge(init_arg)
    |> Horde.Registry.init()
  end

  defp members do
    [Node.self() | Node.list()]
    |> Enum.map(fn node -> {__MODULE__, node} end)
  end
end
