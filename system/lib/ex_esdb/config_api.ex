defmodule ExESDB.ConfigAPI do
  @moduledoc """
  High-level API for configuration management.
  Uses Ra leadership awareness for safe configuration writes.
  """
  
  require Logger
  alias ExESDB.ConfigReader
  alias ExESDB.ConfigWriter
  alias ExESDB.KhepriCluster

  @config_store_id :ex_esdb_config

  @doc """
  Creates or updates a store configuration.
  Only succeeds if called on the Ra leader for the config store.
  """
  @spec put_store_config(atom(), map()) :: {:ok, :created | :updated} | {:error, term()}
  def put_store_config(store_id, config) when is_atom(store_id) and is_map(config) do
    case check_leadership() do
      {:ok, :leader} ->
        # We are the Ra leader, safe to write
        case ConfigReader.read_store_config(store_id) do
          {:ok, _existing} ->
            ConfigWriter.write_store_config(store_id, config)
            {:ok, :updated}
          {:error, :not_found} ->
            ConfigWriter.write_store_config(store_id, config)
            {:ok, :created}
          {:error, reason} ->
            {:error, reason}
        end

      {:ok, leader_node} ->
        # Forward to leader
        Logger.info("Forwarding config write to leader: #{inspect(leader_node)}")
        :rpc.call(leader_node, __MODULE__, :put_store_config, [store_id, config])

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Reads a store configuration.
  Can be called from any node (Ra handles read consistency).
  """
  @spec get_store_config(atom()) :: {:ok, map()} | {:error, term()}
  def get_store_config(store_id) when is_atom(store_id) do
    ConfigReader.read_store_config(store_id)
  end

  @doc """
  Lists all store configurations.
  Can be called from any node.
  """
  @spec list_store_configs() :: {:ok, map()} | {:error, term()}
  def list_store_configs() do
    case ConfigReader.list_store_configs() do
      {:ok, configs} ->
        # Transform list to map for easier consumption
        config_map = 
          configs
          |> Enum.map(fn {path, config} ->
            store_id = List.last(path)
            {store_id, config}
          end)
          |> Map.new()
        {:ok, config_map}
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Deletes a store configuration.
  Only succeeds if called on the Ra leader for the config store.
  """
  @spec delete_store_config(atom()) :: {:ok, :deleted} | {:error, term()}
  def delete_store_config(store_id) when is_atom(store_id) do
    case check_leadership() do
      {:ok, :leader} ->
        # We are the Ra leader, safe to delete
        case ConfigReader.read_store_config(store_id) do
          {:ok, _existing} ->
            ConfigWriter.delete_store_config(store_id)
            {:ok, :deleted}
          {:error, :not_found} ->
            {:error, :not_found}
          {:error, reason} ->
            {:error, reason}
        end

      {:ok, leader_node} ->
        # Forward to leader
        Logger.info("Forwarding config delete to leader: #{inspect(leader_node)}")
        :rpc.call(leader_node, __MODULE__, :delete_store_config, [store_id])

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Gets the current Ra leader for the config store.
  """
  @spec get_config_leader() :: {:ok, node()} | {:error, term()}
  def get_config_leader() do
    case :ra_leaderboard.lookup_leader(@config_store_id) do
      {_, leader_node} when is_atom(leader_node) ->
        {:ok, leader_node}
      :undefined ->
        {:error, :no_leader}
      other ->
        {:error, {:unexpected_leader_response, other}}
    end
  end

  @doc """
  Checks if this node is the Ra leader for the config store.
  """
  @spec is_config_leader?() :: boolean()
  def is_config_leader?() do
    KhepriCluster.leader?(@config_store_id)
  end

  @doc """
  Waits for the config store to have a leader.
  Useful during cluster startup.
  """
  @spec wait_for_leader(timeout()) :: {:ok, node()} | {:error, :timeout}
  def wait_for_leader(timeout \\ 10_000) do
    end_time = System.monotonic_time(:millisecond) + timeout
    wait_for_leader_loop(end_time)
  end

  # Private functions

  defp check_leadership() do
    case :ra_leaderboard.lookup_leader(@config_store_id) do
      {_, leader_node} when leader_node == node() ->
        {:ok, :leader}
      {_, leader_node} when is_atom(leader_node) ->
        {:ok, leader_node}
      :undefined ->
        {:error, :no_leader}
      other ->
        {:error, {:unexpected_leader_response, other}}
    end
  end

  defp wait_for_leader_loop(end_time) do
    case get_config_leader() do
      {:ok, leader_node} ->
        {:ok, leader_node}
      {:error, :no_leader} ->
        if System.monotonic_time(:millisecond) < end_time do
          Process.sleep(100)
          wait_for_leader_loop(end_time)
        else
          {:error, :timeout}
        end
      {:error, reason} ->
        {:error, reason}
    end
  end
end
