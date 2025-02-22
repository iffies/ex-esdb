defmodule Schema.ScarabInit do
  @moduledoc """
  Initialization Parameters for Scarab
  """
  use Ecto.Schema

  import Ecto.Changeset

  require Logger

  @fields [
    :data_dir,
    :store_id,
    :timeout
  ]

  @required_fields []

  embedded_schema do
    field(:data_dir, :string)
    field(:store_id, :string)
    field(:timeout, :integer)
  end

  def changeset(seed, params) when is_struct(params) do
    changeset(seed, Map.from_struct(params))
  end

  def changeset(seed, attrs)
      when is_map(attrs) do
    seed
    |> cast(attrs, @fields)
    |> validate_required(@required_fields)
  end

  def from_map(seed, attrs) do
    case changeset(seed, attrs) do
      %{valid?: true} = changes ->
        {:ok, apply_changes(changes)}

      changes ->
        Logger.error("Invalid parameters: #{inspect(changes, clean: true)}")
        {:error, changes}
    end
  end
end
