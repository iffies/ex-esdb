defmodule ScarabES.Config do
  @moduledoc """
  This module is responsible for loading the configuration for the application.
  """

  require Logger

  @doc """
    Returns the data directory for the application when passed a map or a keyword list.
  """
  def data_dir(config) when is_map(config),
    do:
      config
      |> Map.get(:data_dir) || "data"

  def data_dir(config) when is_list(config),
    do:
      config
      |> Keyword.get(:data_dir) || "data"

  @doc """
    Returns the store id for the application when passed a map or a keyword list.
  """
  def store_id(config) when is_map(config),
    do:
      config
      |> Map.get(:store_id) || :scarab

  def store_id(config) when is_list(config),
    do:
      config
      |> Keyword.get(:store_id) || :scarab

  def timeout(config),
    do:
      config
      |> Keyword.get(:timeout) || 10_000

  def db_type(config),
    do:
      config
      |> Keyword.get(:db_type) || :node

   def khepri_config(config),
    do:
      config
      |> Keyword.get(:khepri) || []

  def fetch_env!() do
    case Application.fetch_env!(:scarab_es, :khepri) do
      nil -> raise(ArgumentError, "no config found!")
      config -> Map.new(config)
    end
  end


  def scarab_seeds()  do 
    seeds =      System.get_env("SCARABES_SEEDS") || to_string(node())
    Logger.info("SCARABES_SEEDS: #{inspect(seeds, pretty: true)}")

    nodes =  seeds
      |> String.trim()
      |> String.downcase()
      |> String.replace(" ", "_")
      |> String.split(",")
      |> Enum.reject(
        fn seed -> 
          seed == "" or 
            seed == "nil" or 
            seed == to_string(node()) 
      end)

    Logger.info("CLUSTER NODES: #{inspect(nodes, pretty: true)}")

    atoms = nodes
      |> Enum.map(
          fn seed ->

          try do
            String.to_existing_atom(seed) 
          rescue
           _ -> String.to_atom(seed)
          end

        end
        )
    Logger.info("CLUSTER ATOMS: #{inspect(atoms, pretty: true)}")
    atoms
  end

end
