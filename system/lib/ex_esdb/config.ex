defmodule ExESDB.Config do
  @moduledoc """
  Configuration validation and normalization for ExESDB.

  This module provides standardized configuration handling with proper validation,
  error handling, and normalization to ensure consistent behavior across the system.
  """

  require Logger

  @type config :: Keyword.t()
  @type store_id :: atom()
  @type data_dir :: String.t()
  @type time_out :: non_neg_integer()
  @type db_type :: :single | :cluster
  @type pub_sub :: atom()
  @type idle_ms :: non_neg_integer()

  @doc """
  Validates and normalizes configuration for ExESDB.

  ## Options

  * `:store_id` - Unique identifier for the store (required)
  * `:data_dir` - Directory for data storage (required)
  * `:timeout` - Operation timeout in milliseconds (default: 10_000)
  * `:db_type` - Database type `:single` or `:cluster` (default: `:single`)
  * `:pub_sub` - PubSub mechanism identifier (default: `:native`)
  * `:writer_idle_ms` - Writer idle timeout in milliseconds (default: 10_000)
  * `:reader_idle_ms` - Reader idle timeout in milliseconds (default: 10_000)
  * `:use_libcluster` - Whether to use libcluster for node discovery (default: `true`)

  ## Examples

      iex> ExESDB.Config.validate([store_id: :my_store, data_dir: "/tmp/data"])
      {:ok, %{store_id: :my_store, data_dir: "/tmp/data", timeout: 10_000, ...}}
      
      iex> ExESDB.Config.validate([])
      {:error, {:missing_required_config, [:store_id, :data_dir]}}
  """
  @spec validate(config()) :: {:ok, map()} | {:error, {atom(), term()}}
  def validate(config) when is_list(config) do
    with {:ok, validated_config} <- validate_required_fields(config),
         {:ok, normalized_config} <- normalize_config(validated_config) do
      {:ok, normalized_config}
    end
  rescue
    error -> {:error, {:validation_error, error}}
  end

  @doc """
  Gets the store ID from configuration or environment.
  """
  @spec store_id(config()) :: store_id()
  def store_id(config) do
    case get_config_value(config, :store_id, "EXESDB_STORE_ID") do
      nil -> raise ArgumentError, "store_id is required"
      value when is_binary(value) -> String.to_atom(value)
      value when is_atom(value) -> value
    end
  end

  @doc """
  Gets the data directory from configuration or environment.
  """
  @spec data_dir(config()) :: data_dir()
  def data_dir(config) do
    case get_config_value(config, :data_dir, "EXESDB_DATA_DIR") do
      nil -> raise ArgumentError, "data_dir is required"
      value when is_binary(value) -> validate_data_dir(value)
    end
  end

  @doc """
  Gets the timeout from configuration or environment.
  """
  @spec timeout(config()) :: time_out()
  def timeout(config) do
    case get_config_value(config, :timeout, "EXESDB_TIMEOUT") do
      nil ->
        10_000

      value when is_integer(value) and value > 0 ->
        value

      value when is_binary(value) ->
        case Integer.parse(value) do
          {int_val, ""} when int_val > 0 -> int_val
          _ -> raise ArgumentError, "timeout must be a positive integer"
        end
    end
  end

  @doc """
  Gets the database type from configuration or environment.
  """
  @spec db_type(config()) :: db_type()
  def db_type(config) do
    case get_config_value(config, :db_type, "EXESDB_DB_TYPE") do
      nil -> :single
      :single -> :single
      :cluster -> :cluster
      "single" -> :single
      "cluster" -> :cluster
      other -> raise ArgumentError, "db_type must be :single or :cluster, got: #{inspect(other)}"
    end
  end

  @doc """
  Gets the PubSub configuration from configuration or environment.
  """
  @spec pub_sub(config()) :: pub_sub()
  def pub_sub(config) do
    case get_config_value(config, :pub_sub, "EXESDB_PUB_SUB") do
      nil -> :native
      value when is_atom(value) -> value
      value when is_binary(value) -> String.to_atom(value)
    end
  end

  @doc """
  Gets the writer idle timeout from configuration or environment.
  """
  @spec writer_idle_ms(config()) :: idle_ms()
  def writer_idle_ms(config) do
    case get_config_value(config, :writer_idle_ms, "EXESDB_WRITER_IDLE_MS") do
      nil ->
        10_000

      value when is_integer(value) and value > 0 ->
        value

      value when is_binary(value) ->
        case Integer.parse(value) do
          {int_val, ""} when int_val > 0 -> int_val
          _ -> raise ArgumentError, "writer_idle_ms must be a positive integer"
        end
    end
  end

  @doc """
  Gets the reader idle timeout from configuration or environment.
  """
  @spec reader_idle_ms(config()) :: idle_ms()
  def reader_idle_ms(config) do
    case get_config_value(config, :reader_idle_ms, "EXESDB_READER_IDLE_MS") do
      nil ->
        10_000

      value when is_integer(value) and value > 0 ->
        value

      value when is_binary(value) ->
        case Integer.parse(value) do
          {int_val, ""} when int_val > 0 -> int_val
          _ -> raise ArgumentError, "reader_idle_ms must be a positive integer"
        end
    end
  end

  @doc """
  Checks if libcluster should be used for node discovery.
  """
  @spec use_libcluster?(config()) :: boolean()
  def use_libcluster?(config) do
    case get_config_value(config, :use_libcluster, "EXESDB_USE_LIBCLUSTER") do
      nil -> true
      value when is_boolean(value) -> value
      "true" -> true
      "false" -> false
      other -> raise ArgumentError, "use_libcluster must be a boolean, got: #{inspect(other)}"
    end
  end

  @doc """
  Validates libcluster topology configuration.
  """
  @spec validate_libcluster_config(config()) :: {:ok, config()} | {:error, term()}
  def validate_libcluster_config(config) do
    if use_libcluster?(config) do
      case Application.get_env(:libcluster, :topologies) do
        nil ->
          Logger.warning("libcluster is enabled but no topologies configured")
          {:ok, config}

        topologies when is_list(topologies) ->
          Logger.info("libcluster enabled with #{length(topologies)} topologies")
          {:ok, config}

        invalid ->
          {:error, {:invalid_libcluster_config, invalid}}
      end
    else
      {:ok, config}
    end
  end

  # Private functions

  defp validate_required_fields(config) do
    required = [:store_id, :data_dir]

    missing =
      Enum.filter(required, fn key ->
        case get_config_value(config, key, nil) do
          nil -> true
          "" -> true
          _ -> false
        end
      end)

    case missing do
      [] -> {:ok, config}
      missing_keys -> {:error, {:missing_required_config, missing_keys}}
    end
  end

  defp normalize_config(config) do
    normalized = %{
      store_id: store_id(config),
      data_dir: data_dir(config),
      timeout: timeout(config),
      db_type: db_type(config),
      pub_sub: pub_sub(config),
      writer_idle_ms: writer_idle_ms(config),
      reader_idle_ms: reader_idle_ms(config),
      use_libcluster: use_libcluster?(config)
    }

    with {:ok, _} <- validate_libcluster_config(config) do
      {:ok, normalized}
    end
  end

  defp get_config_value(config, key, env_var) do
    case Keyword.get(config, key) do
      nil when is_binary(env_var) -> System.get_env(env_var)
      nil -> nil
      value -> value
    end
  end

  defp validate_data_dir(dir) when is_binary(dir) do
    expanded = Path.expand(dir)

    case File.mkdir_p(expanded) do
      :ok ->
        expanded

      {:error, reason} ->
        raise ArgumentError, "unable to create data directory #{expanded}: #{reason}"
    end
  end
end
