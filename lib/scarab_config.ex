defmodule Scarab.Config do
  @moduledoc """
  This module is responsible for loading the configuration for the application.
  """
  def data_dir(cofig) do
    case Keyword.get(cofig, :data_dir) do
      nil -> System.get_env("HOME")
      data_dir -> data_dir
    end
  end

  def store_id(cofig) do
    case Keyword.get(cofig, :store_id) do
      nil -> System.get_env("STORE_ID")
      store_id -> store_id
    end
  end

  def timeout(cofig) do
    Keyword.get(cofig, :timeout) || 5_000
  end

  def serializer(cofig) do
    Keyword.get(cofig, :serializer) ||
      raise ArgumentError, "expected :serializer to be set in config"
  end
end
