defmodule ExESDB.ConfigWriter do
  @moduledoc """
  The API for interacting with ExESDB Config Writers.
  """
  def hr_config_writer_name(store_id),
    do: :"config_writer_#{store_id}"

  def cluster_id(store_id),
    do: {:config_writer, store_id}

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
    key = :erlang.phash2(store_id, partitions)
    key
  end

  defp start_worker(store_id) do
    partition = partition_for(store_id)

    case DynamicSupervisor.start_child(
           {:via, PartitionSupervisor, {ExESDB.ConfigWriters, partition}},
           {ExESDB.ConfigWriterWorker, {store_id, partition}}
         ) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      {:error, reason} -> raise "failed to start config writer: #{inspect(reason)}"
    end
  end

  @spec write_store_config(store_id :: atom(), config :: map()) :: :ok
  def write_store_config(store_id, config) do
    GenServer.cast(
      get_worker(store_id),
      {:write_store_config, store_id, config}
    )

    :ok
  end

  @spec delete_store_config(store_id :: atom()) :: :ok
  def delete_store_config(store_id) do
    GenServer.cast(
      get_worker(store_id),
      {:delete_store_config, store_id}
    )

    :ok
  end
end
