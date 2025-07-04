defmodule ExESDB.ClusterSystem do
  @moduledoc """
  Supervisor for all cluster-related components.
  
  This supervisor manages:
  - ClusterCoordinator: Handles coordination logic and split-brain prevention
  - Cluster: Manages Khepri cluster operations (join, leave, members, leader detection)
  
  The ClusterCoordinator is started first to ensure it's available when the Cluster
  module needs to make coordination decisions.
  """
  use Supervisor

  alias ExESDB.Themes, as: Themes

  @impl true
  def init(opts) do
    children = [
      # Start ClusterCoordinator first so it's available for Cluster
      {ExESDB.ClusterCoordinator, opts},
      {ExESDB.Cluster, opts}
    ]

    IO.puts("#{Themes.cluster_system(self())} is UP")

    Supervisor.init(children, strategy: :one_for_one)
  end

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: :infinity,
      type: :supervisor
    }
  end
end
