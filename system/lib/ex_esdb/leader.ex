defmodule ExESDB.Leader do
  @moduledoc """
    This module supervises the Leader Subsystem.
  """
  use Supervisor
  require Logger
  alias ExESDB.Themes, as: Themes
  ############### PlUMBIng ############
  #
  def start_link(opts),
    do:
      Supervisor.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  @impl true
  def init(config) do
    IO.puts("#{Themes.leader(self())} is UP!")
    Process.flag(:trap_exit, true)

    children = [
      {ExESDB.LeaderWorker, config},
      {ExESDB.SubscriptionsTracker, config}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule ExESDB.LeaderWorker do
  @moduledoc """
    This module contains the leader's reponsibilities for the cluster.
  """
  use GenServer
  require Logger
  alias ExESDB.SubscriptionsReader, as: SubsR
  alias ExESDB.Themes, as: Themes
  alias ExESDB.Emitters
  ############ API ############
  def activate(store),
    do:
      GenServer.cast(
        __MODULE__,
        {:activate, store}
      )

  ########## HANDLE_CAST ##########
  @impl true
  def handle_cast({:activate, store}, state) do
    IO.puts("🚀🚀 Activating LEADER #{inspect(node())} 🚀🚀")

    subscriptions =
      store
      |> SubsR.get_subscriptions()

    if subscriptions |> Enum.empty?(),
      do: IO.puts("😦😦 No subscriptions found. 😦😦")

    subscriptions
    |> Enum.each(fn {_, subscription} ->
      store
      |> Emitters.start_emitter(subscription)
    end)

    {:noreply, state}
  end

  @impl true
  def handle_cast(msg, state) do
    Logger.warning("Leader received unexpected CAST: #{inspect(msg)}")
    {:noreply, state}
  end

  ################ HANDLE_INFO ############
  @impl true
  def handle_info(msg, state) do
    Logger.warning("Leader received unexpected INFO: #{inspect(msg)}")
    {:noreply, state}
  end

  ############# HANDLE_CALL ##########
  @impl true
  def handle_call(msg, _from, state) do
    Logger.warning("Leader received unexpected CALL: #{inspect(msg)}")
    {:reply, :ok, state}
  end

  ############# PLUMBING #############
  #
  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  @impl true
  def terminate(reason, _state) do
    Logger.warning("#{Themes.cluster(self())} terminating with reason: #{inspect(reason)}")
    :ok
  end

  @impl true
  def init(config) do
    IO.puts("#{Themes.leader(self())} is UP!")
    Process.flag(:trap_exit, true)
    {:ok, config}
  end

  def child_spec(opts),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 10_000,
      type: :worker
    }
end
