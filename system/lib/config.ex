defmodule Scarab.Config do
  @moduledoc """
  This module is responsible for loading the configuration for the application.
  """
  def data_dir(config),
    do:
      config
      |> Keyword.get(:data_dir) || "scarab_data"

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
      |> Keyword.get(:timeout) || 5_000

  def db_type(config),
    do:
      config
      |> Keyword.get(:db_type) || :node

   def khepri_config(config),
    do:
      config
      |> Keyword.get(:khepri) || []

  def fetch_env!(app) do
    case Application.fetch_env!(app, :khepri) do
      nil -> raise(ArgumentError, "no config for #{inspect(app)}")
      config -> Map.new(config)
    end
  end


  def scarab_seeds()  do 
      System.get_env("SCARAB_SEEDS") || to_string(node())
      |> String.trim()
      |> String.downcase()
      |> String.replace( " ", "_")
      |> String.replace(~r/[^\w_@]/, "")
      |> String.split(",")
      |> Enum.reject(
        fn seed -> 
          seed == "" or 
            seed == "nil" or 
            seed == to_string(node()) 
      end)
      |> Enum.map(
          fn seed -> 
            case String.to_existing_atom(seed) do
              :error -> String.to_atom(seed)
              atom -> atom
            end
        end
        )
  end

end
