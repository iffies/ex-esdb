defmodule ExESDB.StreamsReaderPool do
  @moduledoc """
    As part of the ExESDB.System,
  """
  use DynamicSupervisor

  require Logger

  ######################## PLUMBING ########################
  @impl true
  def init(opts) do
    Logger.info("📚 StreamsReaderPool #{inspect(self())} is UP with #{inspect(opts)}", component: :streams_reader_pool)
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
