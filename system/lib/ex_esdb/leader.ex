defmodule ExESDB.Leader do
  @moduledoc """
    This module contains the leader's reponsibilities for the cluster.
  """
  use GenServer
  require Logger
  alias ExESDB.Themes, as: Themes
  alias ExESDB.SubscriptionsReader, as: Reader
  alias ExESDB.Emitters
  ############ API ############
  def activate(store),
    do:
      GenServer.cast(
        __MODULE__,
        {:activate, store}
      )

  @impl true
  ########## CALLBACKS ##########
  def handle_cast({:activate, store}, state) do
    IO.puts("🚀🚀 Activating LEADER #{inspect(node())} 🚀🚀")

    subscriptions =
      store
      |> Reader.get_subscriptions()

    if subscriptions |> Enum.empty?(),
      do: IO.puts("😦😦 No subscriptions found. 😦😦")

    subscriptions
    |> Enum.each(fn subscription ->
      IO.puts("😎😎 Starting emitter for #{inspect(subscription)} 😎😎")

      store
      |> Emitters.start_emitter(subscription)
    end)

    {:noreply, state}
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
