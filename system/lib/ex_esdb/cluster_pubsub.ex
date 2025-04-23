defmodule ExESDB.ClusterPubSub do
  @moduledoc """
    Provides functions for working with event store pub/sub.
  """
  use Horde.Registry
  require Logger
  alias ExESDB.Themes, as: Themes

  def start_link(opts) do
    Horde.Registry.start_link(
      __MODULE__,
      [keys: :unique] ++ opts,
      name: __MODULE__
    )
  end

  @impl true
  def init(init_arg) do
    Logger.warning("#{Themes.cluster_pubsub(self())} is UP")

    [members: members()]
    |> Keyword.merge(init_arg)
    |> Horde.Registry.init()
  end

  defp members do
    [Node.self() | Node.list()]
    |> Enum.map(fn node -> {__MODULE__, node} end)
  end
end
