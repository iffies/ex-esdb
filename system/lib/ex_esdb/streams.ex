defmodule ExESDB.Streams do
  @moduledoc false

  def read_events(store, stream_id, start_version, count) do
    start_version..(start_version + count - 1)
    |> Enum.map(fn version ->
      padded_version = ExESDB.VersionFormatter.pad_version(version, 6)

      store
      |> :khepri.get!([:streams, stream_id, padded_version])
    end)
    |> Enum.reject(&is_nil/1)
  end

  def get_streams(store) do
    store
    |> :khepri.get!([:streams])
    |> Enum.reduce([], fn {stream_id, _stream}, acc -> stream_id ++ acc end)
  end

  def stream_exists?(store, stream_id) do
    store
    |> :khepri.exists([:streams, stream_id])
  end

  def stream_forward(store, stream_id, start_version, count) do
    try do
      case store
           |> stream_exists?(stream_id) do
        true ->
          store
          |> read_events(stream_id, start_version, count)

        false ->
          {:error, :stream_not_found}
      end
    rescue
      e -> {:error, e}
    end
  end
end
