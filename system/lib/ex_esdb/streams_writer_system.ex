defmodule ExESDB.StreamsWriterSystem do
  @moduledoc """
    Provides functions for writing streams
  """
  use Supervisor
  require Logger
  alias ExESDB.Options, as: Options
  alias ExESDB.Themes, as: Themes

  def start_link(opts),
    do: Supervisor.start_link(__MODULE__, opts, name: __MODULE__)

  @impl true
  def init(opts) do
    partitions = System.schedulers_online() - 1
    store = store(opts)
    Logger.warning("There are #{partitions} partitions available.")

    children =
      for partition <- 0..(partitions - 1) do
        Supervisor.child_spec(
          {ExESDB.StreamsWriterPool, {partition, opts}},
          id: {:streams_writer_pool, store, partition}
        )
      end

    ret = Supervisor.init(children, strategy: :one_for_one)
    IO.puts("#{Themes.streams_writer_system(self())} is UP on #{inspect(partitions)} partitions.")
    ret
  end

  defp store(opts), do: Keyword.get(opts, :store, Options.store_id())

  # def start_writer_pools(store, pool_size \\ 3) do
  #   for partition <- 0..(System.schedulers_online() - 1) do
  #     DynamicSupervisor.start_child(
  #       {:via, PartitionSupervisor, {ExESDB.StreamsWriterPools, partition}},
  #       {ExESDB.StreamsWriterPool, {store, pool_size}}
  #     )
  #   end
  # end
end
