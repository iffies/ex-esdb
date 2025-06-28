defmodule ExESDB.EnVars do
  @moduledoc """
    This module contains the environment variables that are used by ExESDB
  """
  @doc """
    Returns the data directory. default: `/data`
  """
  def data_dir, do: "EX_ESDB_DATA_DIR"

  @doc """
    Returns the khepri store id. default: `ex_esdb_store`
  """
  def store_id, do: "EX_ESDB_STORE_ID"

  @doc """
    Returns the db type. `single` or `cluster`. default: `single`
  """
  def db_type, do: "EX_ESDB_DB_TYPE"

  @doc """
    Returns the timeout in milliseconds. default: `10_000`
  """
  def timeout, do: "EX_ESDB_TIMEOUT"

  @doc """
    Returns the seed nodes. default: `nil`
  """
  def seed_nodes, do: "EX_ESDB_SEED_NODES"

  @doc """
    Returns the name of the pub/sub. default: `ex_esdb_pub_sub`
  """
  def pub_sub, do: "EX_ESDB_PUB_SUB"

  @doc """
    Returns the idle writers timeout in milliseconds. default: `10_000`
  """
  def writer_idle_ms, do: "EX_ESDB_WRITER_IDLE_MS"

  @doc """
    Returns the idle readers timeout in milliseconds. default: `10_000`
  """
  def reader_idle_ms, do: "EX_ESDB_READER_IDLE_MS"
end
