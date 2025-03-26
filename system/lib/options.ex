defmodule ExESDB.Options do
  @moduledoc """
  This module is responsible for loading the configuration for the application.
  """

  require Logger
  alias ExESDB.EnVars, as: EnVars

  @doc """
    Returns the data directory for the application when passed a map or a keyword list.
  """
  def data_dir(config) when is_map(config),
    do: Map.get(config, :data_dir) || "data"
  def data_dir(config) when is_list(config),
    do: Keyword.get(config, :data_dir) || "data"
  @doc """
    Returns the store id for the application when passed a map or a keyword list.
  """
  def store_id(config) when is_map(config),
    do: Map.get(config, :store_id) || :exesdb
  def store_id(config) when is_list(config),
    do: Keyword.get(config, :store_id) || :exesdb
  @doc """
    Returns the timeout for the application when passed a map or a keyword list.
  """
  def timeout(config) when is_map(config),
    do: Map.get(config, :timeout) || 10_000
  def timeout(config) when is_list(config),
    do: Keyword.get(config, :timeout) || 10_000
  @doc """
    Returns the db type for the application when passed a map or a keyword list.
  """
  def db_type(config) when is_map(config),
    do: Map.get(config, :db_type) || :single
  def db_type(config) when is_list(config),
    do: Keyword.get(config, :db_type) || :single

  def seed_nodes(config) when is_map(config),
    do: Map.get(config, :seed_nodes)
  def seed_nodes(config) when is_list(config),
    do: Keyword.get(config, :seed_nodes)

  defp defaults!,
    do:
      %{
        data_dir: "/data",
        store_id: :ex_esdb,
        db_type: :single,
        timeout: 10_000,
        seed_nodes: nil
      }

  def fetch! do
    case Application.fetch_env!(:ex_esdb, :khepri) do
      nil -> defaults!()
      config -> Map.new(config)
    end
  end

  defp clean_node(node),
    do:
    String.trim(node)
    |> String.downcase()
    |> String.replace(" ", "")
    |> String.replace(",", "")
    |> String.replace(".", "")
    |> String.replace(":", "")

  defp seed_to_atom(seed)  do
    case String.to_existing_atom(seed) do
      :undefined -> String.to_atom(seed)
      atom -> atom
    end
  end

  def seeds do
    seeds =  System.get_env(EnVars.seed_nodes())

    Logger.info("EX_ESDB_SEEDS: #{inspect(seeds, pretty: true)}")
    nodes =
      seeds
      |> String.split(",")
      |> Enum.map(fn _ -> &clean_node/1 end)
      |> Enum.reject(fn seed -> seed == to_string(node()) end)
      |> Enum.map(fn _ -> &seed_to_atom/1       end)

    Logger.info("SEED NODES: #{inspect(nodes, pretty: true)}")

    nodes
  end

end
