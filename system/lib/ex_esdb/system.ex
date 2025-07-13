defmodule ExESDB.System do
  @moduledoc """
    This module is the top level supervisor for the ExESDB system.
    
    It uses a layered supervision architecture for better fault tolerance:
    - CoreSystem: Critical infrastructure (StoreSystem + PersistenceSystem)
    - LeadershipSystem: Leader election and event emission
    - GatewaySystem: External interface with pooled workers
    - Conditional clustering components (LibCluster, ClusterSystem)
    
    Note: Store management is now handled by the distributed ex-esdb-gater API.
  """
  use Supervisor

  alias ExESDB.Options, as: Options
  alias ExESDB.Themes, as: Themes

  require Logger
  require Phoenix.PubSub

  @impl true
  def init(opts) do
    db_type = Options.db_type()

    Logger.info("Starting ExESDB in #{db_type} mode")

    children = [
      # Core infrastructure - must start first
      {ExESDB.CoreSystem, opts},
      # Leadership and event emission
      {ExESDB.LeadershipSystem, opts},
      # External interface
      {ExESDB.GatewaySystem, opts}
    ]

    # Conditionally add clustering components based on db_type
    children =
      case db_type do
        :cluster ->
          topologies = Options.topologies()
          Logger.info("Adding clustering components for cluster mode")

          [
            {Cluster.Supervisor, [topologies, [name: ExESDB.LibCluster]]},
            {ExESDB.ClusterSystem, opts}
          ] ++ children

        :single ->
          Logger.info("Skipping clustering components for single-node mode")
          children

        _ ->
          Logger.warning("Unknown db_type: #{inspect(db_type)}, defaulting to single-node mode")
          children
      end

    :os.set_signal(:sigterm, :handle)
    :os.set_signal(:sigquit, :handle)

    spawn(fn -> handle_os_signal() end)

    ret =
      Supervisor.init(
        children,
        strategy: :rest_for_one
      )

    msg = "is UP in #{db_type} mode!"
    IO.puts("#{Themes.system(self(), msg)}")
    ret
  end

  defp handle_os_signal do
    receive do
      {:signal, :sigterm} ->
        Logger.warning("SIGTERM received. Stopping ExESDB")
        stop(:sigterm)

      {:signal, :sigquit} ->
        Logger.warning("SIGQUIT received. Stopping ExESDB")
        stop(:sigquit)

      msg ->
        IO.puts("Unknown signal: #{inspect(msg)}")
        Logger.warning("Received unknown signal: #{inspect(msg)}")
    end

    handle_os_signal()
  end

  def stop(_reason \\ :normal) do
    Process.sleep(2_000)
    Application.stop(:ex_esdb)
  end

  def start_link(opts),
    do:
      Supervisor.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  def start(opts) do
    case start_link(opts) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      {:error, reason} -> raise "failed to start eventstores supervisor: #{inspect(reason)}"
    end
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :supervisor
    }
  end
end
