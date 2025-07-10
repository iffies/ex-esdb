defmodule ExESDB.ConfigObserver do
  @moduledoc """
  Observes store configuration changes and manages local StoreSystem lifecycle.

  This module:
  - Runs on every node in the cluster  
  - Joins a tracker_group to receive config change notifications
  - Manages local StoreSystem lifecycle (start/stop/update stores on this node)
  - Receives broadcasts from ConfigTracker (via tracker_group notifications)

  The ConfigObserver works in coordination with:
  - ConfigTracker (runs on leader, manages Khepri triggers and broadcasts)
  - tracker_group (provides cluster-wide messaging via pg groups)
  - StoreSupervisor (manages actual store processes on this node)
  """
  use GenServer
  require Logger

  alias ExESDB.Themes

  @feature :stores
  @config_store :config_store

  # Client API

  def start_link(opts \\ []) do
    store_id = Keyword.get(opts, :store_id, @config_store)
    GenServer.start_link(__MODULE__, [store_id: store_id], name: __MODULE__)
  end

  @doc """
  Get information about managed stores on this node.
  """
  def get_managed_stores do
    GenServer.call(__MODULE__, :get_managed_stores)
  end

  @doc """
  Manually sync with existing store configurations.
  Usually called on startup or after network partitions.
  """
  def sync_existing_stores do
    GenServer.cast(__MODULE__, :sync_existing_stores)
  end

  # Server callbacks

  @impl true
  def init(opts) do
    store_id = Keyword.get(opts, :store_id, @config_store)

    state = %{
      store_id: store_id,
      managed_stores: MapSet.new(),
      node: node()
    }

    # Join the tracker group for store configuration events
    :ok = :tracker_group.join(store_id, @feature, self())

    Logger.info(
      "#{Themes.config_observer(self(), "joined tracker group for store #{store_id} on node #{node()}")}"
    )

    # Sync with existing stores after a delay to allow cluster to stabilize
    Process.send_after(self(), :sync_existing_stores, 3000)

    {:ok, state}
  end

  @impl true
  def handle_call(:get_managed_stores, _from, state) do
    managed_list = MapSet.to_list(state.managed_stores)
    {:reply, managed_list, state}
  end

  @impl true
  def handle_cast(:sync_existing_stores, state) do
    send(self(), :sync_existing_stores)
    {:noreply, state}
  end

  @impl true
  def handle_info(:sync_existing_stores, state) do
    Logger.info("#{Themes.config_observer(self(), "syncing with existing store configurations")}")

    case get_existing_store_configs() do
      {:ok, configs} ->
        existing_stores = Map.keys(configs)

        Logger.info(
          "#{Themes.config_observer(self(), "found #{length(existing_stores)} existing stores")}"
        )

        started_stores =
          Enum.reduce(existing_stores, MapSet.new(), fn store_id, acc ->
            config = Map.get(configs, store_id, %{})

            if should_start_store?(config) do
              case start_store_system(store_id, config) do
                :ok -> MapSet.put(acc, store_id)
                {:error, _} -> acc
              end
            else
              acc
            end
          end)

        {:noreply, %{state | managed_stores: started_stores}}

      {:error, reason} ->
        Logger.warning(
          "#{Themes.config_observer(self(), "failed to sync existing stores: #{inspect(reason)}")}"
        )

        # Retry after delay
        Process.send_after(self(), :sync_existing_stores, 10000)
        {:noreply, state}
    end
  end

  # Handle tracker_group notifications from ConfigTracker

  @impl true
  def handle_info({:feature_created, @feature, store_data}, state) do
    store_id = extract_store_id(store_data)
    config = extract_store_config(store_data)

    Logger.info(
      "#{Themes.config_observer(self(), "received feature_created for store #{store_id}")}"
    )

    if should_start_store?(config) do
      case start_store_system(store_id, config) do
        :ok ->
          new_managed = MapSet.put(state.managed_stores, store_id)
          {:noreply, %{state | managed_stores: new_managed}}

        {:error, reason} ->
          Logger.error(
            "#{Themes.config_observer(self(), "failed to start store #{store_id}: #{inspect(reason)}")}"
          )

          {:noreply, state}
      end
    else
      Logger.info(
        "#{Themes.config_observer(self(), "store #{store_id} configured not to auto-start")}"
      )

      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:feature_updated, @feature, store_data}, state) do
    store_id = extract_store_id(store_data)
    config = extract_store_config(store_data)

    Logger.info(
      "#{Themes.config_observer(self(), "received feature_updated for store #{store_id}")}"
    )

    cond do
      # Store should be running and is currently managed - update it
      should_start_store?(config) and MapSet.member?(state.managed_stores, store_id) ->
        case update_store_system(store_id, config) do
          :ok ->
            Logger.info(
              "#{Themes.config_observer(self(), "updated configuration for store #{store_id}")}"
            )

            {:noreply, state}

          {:error, reason} ->
            Logger.error(
              "#{Themes.config_observer(self(), "failed to update store #{store_id}: #{inspect(reason)}")}"
            )

            {:noreply, state}
        end

      # Store should be running but isn't currently managed - start it
      should_start_store?(config) and not MapSet.member?(state.managed_stores, store_id) ->
        case start_store_system(store_id, config) do
          :ok ->
            new_managed = MapSet.put(state.managed_stores, store_id)

            Logger.info(
              "#{Themes.config_observer(self(), "started store #{store_id} due to config update")}"
            )

            {:noreply, %{state | managed_stores: new_managed}}

          {:error, reason} ->
            Logger.error(
              "#{Themes.config_observer(self(), "failed to start store #{store_id}: #{inspect(reason)}")}"
            )

            {:noreply, state}
        end

      # Store should not be running but is currently managed - stop it  
      not should_start_store?(config) and MapSet.member?(state.managed_stores, store_id) ->
        case stop_store_system(store_id) do
          :ok ->
            new_managed = MapSet.delete(state.managed_stores, store_id)

            Logger.info(
              "#{Themes.config_observer(self(), "stopped store #{store_id} due to config update")}"
            )

            {:noreply, %{state | managed_stores: new_managed}}

          {:error, reason} ->
            Logger.error(
              "#{Themes.config_observer(self(), "failed to stop store #{store_id}: #{inspect(reason)}")}"
            )

            {:noreply, state}
        end

      # Store should not be running and isn't managed - no action needed
      true ->
        Logger.debug(
          "#{Themes.config_observer(self(), "store #{store_id} update requires no local action")}"
        )

        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:feature_deleted, @feature, store_data}, state) do
    store_id = extract_store_id(store_data)

    Logger.info(
      "#{Themes.config_observer(self(), "received feature_deleted for store #{store_id}")}"
    )

    if MapSet.member?(state.managed_stores, store_id) do
      case stop_store_system(store_id) do
        :ok ->
          new_managed = MapSet.delete(state.managed_stores, store_id)

          Logger.info(
            "#{Themes.config_observer(self(), "stopped and removed store #{store_id}")}"
          )

          {:noreply, %{state | managed_stores: new_managed}}

        {:error, reason} ->
          Logger.error(
            "#{Themes.config_observer(self(), "failed to stop store #{store_id}: #{inspect(reason)}")}"
          )

          {:noreply, state}
      end
    else
      Logger.debug(
        "#{Themes.config_observer(self(), "store #{store_id} was not managed on this node")}"
      )

      {:noreply, state}
    end
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("#{Themes.config_observer(self(), "received unknown message: #{inspect(msg)}")}")
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    Logger.info(
      "#{Themes.config_observer(self(), "terminating with reason: #{inspect(reason)}")}"
    )

    # Leave the tracker group
    :tracker_group.leave(state.store_id, @feature, self())

    # Optionally stop managed stores on shutdown (usually not needed)
    if reason not in [:shutdown, :normal] do
      Enum.each(state.managed_stores, fn store_id ->
        Logger.info(
          "#{Themes.config_observer(self(), "stopping store #{store_id} due to observer termination")}"
        )

        stop_store_system(store_id)
      end)
    end

    :ok
  end

  # Private functions

  defp extract_store_id(store_data) do
    # Extract store_id from the store data map
    # This depends on how the data is structured in the Khepri triggers
    Map.get(store_data, :store_id) || Map.get(store_data, "store_id")
  end

  defp extract_store_config(store_data) do
    # Extract config from the store data map  
    Map.get(store_data, :config, %{}) || Map.get(store_data, "config", %{})
  end

  defp should_start_store?(config) do
    # Check if store should auto-start (default true)
    Map.get(config, :auto_start, true) || Map.get(config, "auto_start", true)
  end

  defp get_existing_store_configs do
    # TODO: This will use the ConfigAPI to get existing store configurations
    # For now, return empty to avoid dependency issues during initial development
    {:ok, %{}}
  end

  defp start_store_system(store_id, config) do
    Logger.info("#{Themes.config_observer(self(), "starting StoreSystem for #{store_id}")}")

    # TODO: Replace with actual store starting logic via StoreSupervisor
    case start_store_via_supervisor(store_id, config) do
      {:ok, _pid} ->
        Logger.info(
          "#{Themes.config_observer(self(), "successfully started StoreSystem for #{store_id}")}"
        )

        :ok

      {:error, {:already_started, _pid}} ->
        Logger.info(
          "#{Themes.config_observer(self(), "StoreSystem for #{store_id} already running")}"
        )

        :ok

      {:error, reason} ->
        Logger.error(
          "#{Themes.config_observer(self(), "failed to start StoreSystem for #{store_id}: #{inspect(reason)}")}"
        )

        {:error, reason}
    end
  end

  defp update_store_system(store_id, config) do
    Logger.info("#{Themes.config_observer(self(), "updating StoreSystem for #{store_id}")}")

    # For now, implement update as stop + start
    # Later this could be enhanced for hot config updates if needed
    case stop_store_system(store_id) do
      :ok -> start_store_system(store_id, config)
      error -> error
    end
  end

  defp stop_store_system(store_id) do
    Logger.info("#{Themes.config_observer(self(), "stopping StoreSystem for #{store_id}")}")

    # TODO: Replace with actual store stopping logic via StoreSupervisor
    case stop_store_via_supervisor(store_id) do
      :ok ->
        Logger.info(
          "#{Themes.config_observer(self(), "successfully stopped StoreSystem for #{store_id}")}"
        )

        :ok

      {:error, :not_found} ->
        Logger.info(
          "#{Themes.config_observer(self(), "StoreSystem for #{store_id} was not running")}"
        )

        :ok

      {:error, reason} ->
        Logger.error(
          "#{Themes.config_observer(self(), "failed to stop StoreSystem for #{store_id}: #{inspect(reason)}")}"
        )

        {:error, reason}
    end
  end

  # Placeholder functions - replace with actual supervisor integration
  defp start_store_via_supervisor(_store_id, _config) do
    # TODO: Integrate with actual StoreSupervisor
    # This should call something like: ExESDB.StoreSupervisor.start_store(store_id, config)
    {:ok, self()}
  end

  defp stop_store_via_supervisor(_store_id) do
    # TODO: Integrate with actual StoreSupervisor  
    # This should call something like: ExESDB.StoreSupervisor.stop_store(store_id)
    :ok
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 10_000,
      type: :worker
    }
  end
end
