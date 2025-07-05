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
      # Start ClusterCoordinator first so it's available for KhepriCluster
      {ExESDB.ClusterCoordinator, opts},
      # Start NodeMonitor for fast failure detection
      {ExESDB.NodeMonitor, node_monitor_config(opts)},
      {ExESDB.KhepriCluster, opts}
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

  # Helper function to configure NodeMonitor options
  defp node_monitor_config(opts) do
    store_id = Keyword.get(opts, :store_id, :ex_esdb)

    # More lenient configuration for node monitoring to prevent cascading failures
    [
      store_id: store_id,
      # 5 seconds (less frequent probing)
      probe_interval: 5_000,
      # 6 consecutive failures (more tolerance)
      failure_threshold: 6,
      # 3 second timeout per probe (more time)
      probe_timeout: 3_000
    ]
  end
end
