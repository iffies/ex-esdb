defmodule ExESDB.ConfigReader do
  @moduledoc """
  Provides functions for reading store configurations
  """

  def hr_config_reader_name(store_id),
    do: :"#{store_id}_config_reader"

  def cluster_id(store_id),
    do: {:config_reader, store_id}

  @spec worker_pids(store_id :: atom()) :: [pid()]
  defp worker_pids(store_id) do
    cluster_id = cluster_id(store_id)

    Swarm.registered()
    |> Enum.filter(fn {name, _} -> match?(^cluster_id, name) end)
    |> Enum.map(fn {_, pid} -> pid end)
  end

  @spec get_worker(store_id :: atom()) :: pid()
  defp get_worker(store_id) do
    case worker_pids(store_id) do
      [] ->
        start_worker(store_id)

      worker_pids ->
        worker_pids
        |> Enum.random()
    end
  end

  defp partition_for(store_id) do
    partitions = System.schedulers_online()
    partition = :erlang.phash2(store_id, partitions)
    partition
  end

  def start_worker(store_id) do
    partition = partition_for(store_id)

    case DynamicSupervisor.start_child(
           {:via, PartitionSupervisor, {ExESDB.ConfigReaders, partition}},
           {ExESDB.ConfigReaderWorker, {store_id, partition}}
         ) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      {:error, reason} -> raise "failed to start config reader: #{inspect(reason)}"
    end
  end

  @doc """
  Reads a store configuration from the config store
  """
  @spec read_store_config(store_id :: atom()) :: {:ok, map()} | {:error, term()}
  def read_store_config(store_id) do
    GenServer.call(
      get_worker(store_id),
      {:read_store_config, store_id}
    )
  end

  @doc """
  Lists all store configurations
  """
  @spec list_store_configs() :: {:ok, [map()]} | {:error, term()}
  def list_store_configs() do
    GenServer.call(
      get_worker(:config_list),
      {:list_store_configs}
    )
  end
end
