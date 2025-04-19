defmodule ExESDB.Emitter do
  @moduledoc """
    As part of the ExESDB.System, 
    the Emitter is responsible for managing the communication 
    between the Event Store and the PubSub mechanism.
  """
  use GenServer

  alias Phoenix.PubSub, as: PubSub

  require Logger

  def emit(store, pub_sub, event),
    do:
      pub_sub
      |> PubSub.broadcast("#{store}", %{event: event})

  def emit!(store, pub_sub, event),
    do:
      pub_sub
      |> PubSub.broadcast!("#{store}", %{event: event})

  @impl GenServer
  def init(opts) do
    Logger.warning("#{Themes.emitter(self())} is UP")
    {:ok, opts}
  end

  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  def child_spec(opts),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 10_000,
      type: :worker
    }
end
