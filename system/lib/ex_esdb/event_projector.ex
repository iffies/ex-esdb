defmodule ExESDB.EventProjector do
  @moduledoc """
    This module contains the event projector functionality
  """
  use GenServer

  alias Phoenix.PubSub, as: PubSub
  import Colors

  @impl true
  def handle_info({:event, event}, state) do
    IO.puts("Event Projector handle :event #{inspect(event, pretty: true)}")
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    IO.puts("Event Projector handle :DOWN")
    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    IO.puts("Event Projector handle random message: #{inspect(msg, pretty: true)}")
    {:noreply, state}
  end


  ##### PLUMBING #####
  @impl true
  def init(opts) do
    IO.puts("#{projector_theme(self())} Event Projector is UP with config: #{inspect(opts, pretty: true)}")
    opts[:pub_sub]
    |> PubSub.subscribe(to_string(opts[:store_id]))
    {:ok, opts}
  end


  def start_link(opts) do
    GenServer.start_link(
      __MODULE__, 
      opts, 
      name: __MODULE__
    )
  end



  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5000,
    }
  end

end
