defmodule Scarab.EventStoreInfo do
  @moduledoc false

  import Scarab.Khepri.Conditions

  def get_streams!(store) do
    store
    |> :khepri.fold(
      [:streams, if_node_exists(exists: true)],
      fn path, props, acc -> acc ++ [{path, props}] end,
      []
    )
  end
end
