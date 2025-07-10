defmodule ExESDB.ConfigTracker do
  @moduledoc """
    As part of the ExESDB.System, the ConfigTracker is responsible for
    observing the configurations that are maintained in the Store.

    Since Khepri triggers are executed on the leader node, the ConfigTracker
    will be instructed to handle configuration changes on the leader node whenever a new config
    is registered, updated, or deleted.

    When a Config is created, updated, or deleted, the ConfigTracker will handle the
    appropriate actions for configuration management.
  """

  use GenServer

  alias ExESDB.KhepriCluster, as: KhepriCluster
  alias ExESDB.Themes, as: Themes

  ########### HANDLE_INFO ###########
  @impl GenServer
  def handle_info({:config_created, data}, state) do
    IO.puts("Config #{inspect(data)} created")
    store = state[:store_id]

    if KhepriCluster.leader?(store) do
      # Handle config creation
      IO.puts("Processing config creation on leader for #{inspect(data)}")
      # Add specific config creation logic here
    end

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:config_updated, data}, state) do
    IO.puts("Config #{inspect(data)} updated")

    if KhepriCluster.leader?(state[:store_id]) do
      # Handle config update
      IO.puts("Processing config update on leader for #{inspect(data)}")
      # Add specific config update logic here
    end

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:config_deleted, data}, state) do
    IO.puts("Config #{inspect(data)} deleted")

    if KhepriCluster.leader?(state[:store_id]) do
      # Handle config deletion
      IO.puts("Processing config deletion on leader for #{inspect(data)}")
      # Add specific config deletion logic here
    end

    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    IO.puts("#{Themes.config_tracker(pid, "exited with reason: #{inspect(reason)}")}")
    store = state[:store_id]

    store
    |> :configs_group.leave(self())

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(_, state) do
    {:noreply, state}
  end

  ############## PLUMBING ##############
  @impl GenServer
  def init(opts) do
    Process.flag(:trap_exit, true)
    store = Keyword.get(opts, :store_id)
    IO.puts("#{Themes.config_tracker(self(), "is UP.")}")

    :ok =
      store
      |> :configs.setup_tracking(self())

    {:ok, opts}
  end

  @impl true
  def terminate(reason, state) do
    IO.puts("#{Themes.config_tracker(self(), "terminating with reason: #{inspect(reason)}")}")
    store = state[:store_id]

    store
    |> :configs_group.leave(self())

    :ok
  end

  def child_spec(opts),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5000
    }

  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )
end
