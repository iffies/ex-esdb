defmodule ExESDB.PartisanCluster do
  @moduledoc """
  Manages Partisan-based clustering for ExESDB servers.

  This module handles:
  - Auto-discovery of ExESDB server nodes
  - Cluster membership management
  - Integration with Ra/Khepri clustering
  - Self-healing capabilities
  """

  use GenServer
  require Logger

  alias ExESDB.Options

  @service_name "ex_esdb_server"
  @discovery_interval 5_000
  @cluster_check_interval 10_000

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def get_cluster_members do
    GenServer.call(__MODULE__, :get_cluster_members)
  end

  def get_server_nodes do
    GenServer.call(__MODULE__, :get_server_nodes)
  end

  def join_cluster(node_spec) do
    GenServer.call(__MODULE__, {:join_cluster, node_spec})
  end

  def leave_cluster do
    GenServer.call(__MODULE__, :leave_cluster)
  end

  # Server Implementation

  @impl true
  def init(opts) do
    # Start Partisan peer service
    case start_partisan() do
      :ok ->
        # Register this node as an ExESDB server
        :ok = register_as_server()

        # Schedule periodic cluster checks
        schedule_cluster_check()

        state = %{
          opts: opts,
          cluster_members: [],
          last_member_count: 0
        }

        Logger.info("ExESDB Partisan cluster manager started")
        {:ok, state}

      {:error, reason} ->
        Logger.error("Failed to start Partisan: #{inspect(reason)}")
        {:stop, reason}
    end
  end

  @impl true
  def handle_call(:get_cluster_members, _from, state) do
    members = get_partisan_members()
    {:reply, members, %{state | cluster_members: members}}
  end

  @impl true
  def handle_call(:get_server_nodes, _from, state) do
    server_nodes = get_server_nodes_from_members()
    {:reply, server_nodes, state}
  end

  @impl true
  def handle_call({:join_cluster, node_spec}, _from, state) do
    result = :partisan_peer_service.join(node_spec)
    {:reply, result, state}
  end

  @impl true
  def handle_call(:leave_cluster, _from, state) do
    result = :partisan_peer_service.leave()
    {:reply, result, state}
  end

  @impl true
  def handle_info(:cluster_check, state) do
    current_members = get_partisan_members()

    # Check if cluster membership changed
    if length(current_members) != state.last_member_count do
      Logger.info("Cluster membership changed. Current members: #{inspect(current_members)}")

      # Update Khepri cluster configuration if needed
      update_khepri_cluster(current_members)

      # Notify other systems about cluster changes
      broadcast_cluster_change(current_members)
    end

    # Schedule next check
    schedule_cluster_check()

    {:noreply,
     %{state | cluster_members: current_members, last_member_count: length(current_members)}}
  end

  @impl true
  def handle_info({:partisan_peer_service_event, :node_up, node}, state) do
    Logger.info("Partisan: Node joined cluster: #{inspect(node)}")

    # Check if it's a server node and update accordingly
    server_nodes = get_server_nodes_from_members()
    update_khepri_cluster(server_nodes)

    {:noreply, state}
  end

  @impl true
  def handle_info({:partisan_peer_service_event, :node_down, node}, state) do
    Logger.info("Partisan: Node left cluster: #{inspect(node)}")

    # Update cluster configuration
    server_nodes = get_server_nodes_from_members()
    update_khepri_cluster(server_nodes)

    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("Unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private Functions

  defp start_partisan do
    case :partisan_peer_service.start_link() do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> :ok
      error -> error
    end
  end

  defp register_as_server do
    # Add metadata to identify this node as a server
    node_spec = %{
      node: Node.self(),
      listen_addrs: get_listen_addresses(),
      metadata: %{
        service: @service_name,
        type: :server,
        store_id: Options.store_id(),
        version: Application.spec(:ex_esdb, :vsn) || "unknown"
      }
    }

    :partisan_peer_service.join(node_spec)
  end

  defp get_listen_addresses do
    # Get the addresses this node listens on
    case :inet.getif() do
      {:ok, interfaces} ->
        interfaces
        |> Enum.map(fn {ip, _broadcast, _netmask} -> :inet.ntoa(ip) end)
        |> Enum.map(&List.to_string/1)

      {:error, _} ->
        ["127.0.0.1"]
    end
  end

  defp get_partisan_members do
    case :partisan_peer_service.members() do
      {:ok, members} -> members
      {:error, _} -> []
    end
  end

  defp get_server_nodes_from_members do
    get_partisan_members()
    |> Enum.filter(fn
      %{metadata: %{service: @service_name, type: :server}} -> true
      _ -> false
    end)
    |> Enum.map(fn %{node: node} -> node end)
  end

  defp update_khepri_cluster(server_nodes) do
    # Only update if we have multiple nodes
    if length(server_nodes) > 1 do
      Logger.info("Updating Khepri cluster with nodes: #{inspect(server_nodes)}")

      # Get current Khepri configuration
      current_config = Options.app_env()
      khepri_config = current_config[:khepri] || []

      # Update seed nodes with discovered servers
      other_nodes = server_nodes -- [Node.self()]

      if length(other_nodes) > 0 do
        updated_config = Keyword.put(khepri_config, :seed_nodes, other_nodes)

        # Restart Khepri with new configuration if needed
        try do
          # This would require implementing a dynamic reconfiguration
          # For now, we'll log the intention
          Logger.info("Would update Khepri config: #{inspect(updated_config)}")
        rescue
          error ->
            Logger.error("Failed to update Khepri cluster: #{inspect(error)}")
        end
      end
    end
  end

  defp broadcast_cluster_change(members) do
    # Broadcast cluster changes to other systems
    Phoenix.PubSub.broadcast(
      :ex_esdb_pubsub,
      "cluster:changes",
      {:cluster_membership_changed, members}
    )
  end

  defp schedule_cluster_check do
    Process.send_after(self(), :cluster_check, @cluster_check_interval)
  end
end
