defmodule ExESDB.Options do
  @moduledoc false
  
  alias ExESDB.EnVars, as: EnVars

  def esdb_khepri, do: Application.get_env(:ex_esdb, :khepri)
  def esdb_khepri(key), do: esdb_khepri()[key]

  def data_dir do
    case System.get_env(EnVars.data_dir()) do
      nil -> esdb_khepri(:data_dir) || "/data"
      data_dir -> data_dir
    end
  end

  def store_id do
    case System.get_env(EnVars.store_id()) do
      nil -> esdb_khepri(:store_id) || :ex_store
      store_id -> String.to_atom(store_id)
    end
  end

  def timeout do
    case System.get_env(EnVars.timeout()) do
      nil -> esdb_khepri(:timeout) || 10_000
      timeout -> String.to_integer(timeout)
    end
  end

  def db_type do
    case System.get_env(EnVars.db_type()) do
      nil -> esdb_khepri(:db_type) || :single
      db_type -> String.to_atom(db_type)
    end
  end

  def seed_nodes do
    case System.get_env(EnVars.seed_nodes()) do
      nil -> esdb_khepri(:seeds_nodes) || [node()]
      seeds -> to_atoms_list(seeds)
    end
  end

  defp to_atoms_list(seeds) do
    seeds
    |> String.split(",")
    |> Enum.map(&clean_node/1)
    |> Enum.map(&seed_to_atom/1)
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
    try do 
      String.to_existing_atom(seed) 
    rescue
      _ -> String.to_atom(seed)
    end
  end

end
