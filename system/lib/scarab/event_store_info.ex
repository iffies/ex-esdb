defmodule Scarab.EventStoreInfo do
  @moduledoc false

  import Scarab.Khepri.Conditions

  def get_streams_raw!(store) do
    case store
         |> :khepri.fold(
           [:streams, if_node_exists(exists: true)],
           fn path, props, acc -> acc ++ [{path, props}] end,
           [],
           %{props_to_return: [:child_list_length]}
         ) do
      {:ok, streams} -> streams
      result -> result
    end
  end

  def get_streams!(store) do
    store
    |> get_streams_raw!()
    |> Stream.map(fn {[:streams, stream_name], %{child_list_length: nbr_of_events}} ->
      [stream_name, nbr_of_events]
    end)
    |> Enum.to_list()
  end
end
