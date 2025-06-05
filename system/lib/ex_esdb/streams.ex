defmodule ExESDB.Streams do
  @moduledoc """
    The ExESDB Streams SubSystem.
  """
  use Supervisor

  require Logger
  alias ExESDB.Themes, as: Themes

  def start_link(opts),
    do:
      Supervisor.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  @impl true
  def init(opts) do
    Logger.warning("#{Themes.streams(self())} is UP.")

    children = [
      {ExESDB.StreamsReader, opts},
      {ExESDB.StreamsWriter, opts}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
