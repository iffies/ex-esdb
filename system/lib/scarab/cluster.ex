defmodule Scarab.Cluster do
  @moduledoc false
  def join(store, cluster),
    do:
      store
      |> :khepri_cluster.join(cluster)

  def leave,
    do: :khepri_cluster.reset()
end
