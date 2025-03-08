defmodule Scarab.EventStreamReader do
  @moduledoc false

  import Scarab.Khepri.IfPathMatches

  def get_current_version(store, stream_id) do
    case store
         |> :khepri.count([
           :streams,
           stream_id,
           if_path_matches(regex: :any)
         ]) do
      {:ok, count} -> count
      _ -> 0
    end
  end

  def read_events(store, stream_id, start_version, count) do
    start_version..(start_version + count - 1)
    |> Enum.map(fn version ->
      padded_version = Scarab.VersionFormatter.pad_version(version, 6)

      store
      |> :khepri.get!([:streams, stream_id, padded_version])
    end)
    |> Enum.reject(&is_nil/1)
  end
end
