defmodule ExESDB.ClusterCoordinator do
  @moduledoc """
  GenServer responsible for coordinating Khepri cluster formation and preventing split-brain scenarios.

  This module handles:
  - Detecting existing clusters
  - Coordinator election
  - Coordinated cluster joining
  - Split-brain prevention
  """
  use GenServer
  require Logger

  alias ExESDB.Themes, as: Themes

  @doc """
  Attempts to join a Khepri cluster using coordinated approach to prevent split-brain.
  Returns one of: :ok, :coordinator, :no_nodes, :waiting, :failed
  """
  def join_cluster(store) do
    GenServer.call(__MODULE__, {:join_cluster, store}, 10_000)
  end

  @doc """
  Checks if this node should handle nodeup events (i.e., not already in a cluster)
  """
  def should_handle_nodeup?(store) do
    GenServer.call(__MODULE__, {:should_handle_nodeup, store}, 5_000)
  end

  ## GenServer Implementation

  @impl true
  def init(_opts) do
    IO.puts("#{Themes.cluster_coordinator(self())} is UP")
    {:ok, %{}}
  end

  @impl true
  def handle_call({:join_cluster, store}, _from, state) do
    result = join_via_connected_nodes(store)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:should_handle_nodeup, store}, _from, state) do
    result = should_handle_nodeup_event?(store)
    {:reply, result, state}
  end

  ## Private Functions

  defp join_via_connected_nodes(store) do
    # Get all connected nodes from LibCluster
    connected_nodes = Node.list()

    if Enum.empty?(connected_nodes) do
      Logger.info(
        "#{Themes.cluster_coordinator(node())} => No connected nodes found via LibCluster, starting as single node cluster"
      )

      :no_nodes
    else
      Logger.info(
        "#{Themes.cluster_coordinator(node())} => Attempting to join Khepri cluster via LibCluster discovered nodes: #{inspect(connected_nodes)}"
      )

      # Find nodes that already have Khepri clusters running
      cluster_nodes = find_existing_cluster_nodes(store, connected_nodes)

      case cluster_nodes do
        [] ->
          # No existing clusters found, check if we should be the coordinator
          handle_no_existing_clusters(connected_nodes)

        [target_node | _] ->
          # Found existing cluster, join it
          join_existing_cluster(store, target_node)
      end
    end
  end

  defp handle_no_existing_clusters(connected_nodes) do
    if should_be_cluster_coordinator(connected_nodes) do
      Logger.info(
        "#{Themes.cluster_coordinator(node())} => Elected as cluster coordinator, starting new cluster"
      )

      :coordinator
    else
      Logger.info(
        "#{Themes.cluster_coordinator(node())} => Waiting for cluster coordinator to establish cluster"
      )

      :waiting
    end
  end

  defp join_existing_cluster(store, target_node) do
    Logger.debug(
      "#{Themes.cluster_coordinator(node())} => Joining existing Khepri cluster via: #{inspect(target_node)}"
    )

    case :khepri_cluster.join(store, target_node) do
      :ok ->
        Logger.info(
          "#{Themes.cluster_coordinator(node())} => Successfully joined existing Khepri cluster via #{inspect(target_node)}"
        )

        :ok

      {:error, reason} ->
        Logger.debug(
          "#{Themes.cluster_coordinator(node())} => Failed to join via #{inspect(target_node)}: #{inspect(reason)}"
        )

        :failed
    end
  end

  defp find_existing_cluster_nodes(store, connected_nodes) do
    connected_nodes
    |> Enum.filter(fn node ->
      try do
        # Check if the node has an active Khepri cluster
        case :rpc.call(node, :khepri_cluster, :members, [store], 5000) do
          {:ok, members} when length(members) > 0 -> true
          _ -> false
        end
      rescue
        _ -> false
      catch
        _, _ -> false
      end
    end)
  end

  defp should_be_cluster_coordinator(connected_nodes) do
    # Use deterministic election: lowest node name becomes coordinator
    all_nodes = [node() | connected_nodes] |> Enum.sort()
    node() == List.first(all_nodes)
  end

  defp should_handle_nodeup_event?(store) do
    # Check if we're already part of a cluster
    case :khepri_cluster.members(store) do
      {:ok, members} when length(members) > 1 ->
        # We're already in a cluster, no need to handle nodeup
        false

      _ ->
        # We're not in a cluster or only have ourselves, should handle nodeup
        true
    end
  end

  ## Child Spec and Startup

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 5_000,
      type: :worker
    }
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
end
