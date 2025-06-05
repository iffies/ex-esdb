defmodule ExESDB.System do
  @moduledoc """
    This module is the top level supervisor for the ExESDB system.
    It is responsible for supervising:
    - The PubSub mechanism
    - the Event Store (starts and stops khepri)
    - the Cluster (joins and leaves the cluster)
    - the Leader (manages Ra leader-specific functionality)
    - the Subscriptions Supervisor (manages subscriptions)
  """
  use Supervisor

  alias ExESDB.Themes, as: Themes

  require Logger
  require Phoenix.PubSub

  @impl true
  def init(opts) do
    Logger.warning("#{Themes.system(self())} is UP")

    children = [
      add_pub_sub(opts),
      {ExESDB.Store, opts},
      {ExESDB.Cluster, opts},
      {ExESDB.Leader, opts},
      {ExESDB.Streams, opts},
      {ExESDB.Subscriptions, opts},
      {PartitionSupervisor, child_spec: DynamicSupervisor, name: ExESDB.EmitterPools},
      {ExESDB.Gateway, opts}
    ]

    :os.set_signal(:sigterm, :handle)
    :os.set_signal(:sigquit, :handle)

    spawn(fn -> handle_os_signal() end)

    Supervisor.init(
      children,
      strategy: :one_for_one
    )
  end

  defp add_pub_sub(opts) do
    pub_sub = Keyword.get(opts, :pub_sub)

    case pub_sub do
      nil ->
        add_pub_sub([pub_sub: :native] ++ opts)

      :native ->
        {ExESDB.PubSub, opts}

      _ ->
        {Phoenix.PubSub, name: pub_sub}
    end
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
