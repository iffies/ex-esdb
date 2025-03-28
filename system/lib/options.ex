defmodule ExESDB.Options do
  @moduledoc """
    This module contains the options helper functions for ExESDB
  """
  
  alias ExESDB.EnVars, as: EnVars

  @data_dir EnVars.data_dir()
  @store_id EnVars.store_id()
  @timeout EnVars.timeout()
  @db_type EnVars.db_type()
  @seed_nodes EnVars.seed_nodes()
  @pub_sub EnVars.pub_sub()


  def sys_env(key), do: System.get_env(key)
  def app_env, do: Application.get_env(:ex_esdb, :khepri)
  def app_env(key), do: app_env()[key]

  def data_dir do
    case sys_env(@data_dir) do
      nil -> app_env(:data_dir) || "/data"
      data_dir -> data_dir
    end
  end

  def store_id do
    case sys_env(@store_id) do
      nil -> app_env(:store_id) || :ex_esdb_store
      store_id -> to_unique_atom(store_id)
    end
  end

  def timeout do
    case sys_env(@timeout) do
      nil -> app_env(:timeout) || 10_000
      timeout -> String.to_integer(timeout)
    end
  end

  def db_type do
    case sys_env(@db_type) do
      nil -> app_env(:db_type) || :single
      db_type -> String.to_atom(db_type)
    end
  end

  def seed_nodes do
    case sys_env(@seed_nodes) do
      nil -> app_env(:seeds) || [node()]
      seeds -> to_atoms_list(seeds)
    end
  end

  def pub_sub do
    case sys_env(@pub_sub) do
      nil -> app_env(:pub_sub) || :ex_esdb_pub_sub
      pub_sub -> to_unique_atom(pub_sub)
    end
  end

  defp to_atoms_list(seeds) do
    seeds
    |> String.split(",")
    |> Enum.map(&clean_node/1)
    |> Enum.map(&to_unique_atom/1)
  end

  defp clean_node(node),
    do:
    String.trim(node)
    |> String.downcase()
    |> String.replace(" ", "")
    |> String.replace(",", "")
    |> String.replace(".", "")
    |> String.replace(":", "")

  defp to_unique_atom(candidate)  do
    try do 
      String.to_existing_atom(candidate) 
    rescue
      _ -> String.to_atom(candidate)
    end
  end

end
