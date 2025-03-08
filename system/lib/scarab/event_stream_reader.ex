defmodule Scarab.EventStreamReader do
  @moduledoc false

  import Scarab.Khepri.IfPathMatches

  def get_current_version(store, stream_id) do
    case store
         |> :khepri.get([
           :streams,
           stream_id,
           if_path_matches(regex: :any)
         ]) do
      {:ok, {_, metadata}} -> metadata[:version] || 0
      _ -> 0
    end
  end
end
