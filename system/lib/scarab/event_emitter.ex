defmodule Scarab.EventEmitter do
  @moduledoc false

  alias Phoenix.PubSub, as: PubSub

  require Logger

  def emit(store, event),
    do:
      :scarab_pubsub
      |> PubSub.broadcast("#{store}", %{event: event})

  def emit!(store, event),
    do:
      :scarab_pubsub
      |> PubSub.broadcast!("#{store}", %{event: event})

  def on_new_event(store, props) do 
    IO.puts("Seen New event: #{inspect(props, pretty: true)}")
    store
    |> emit(props)
  end

  def register_emitter(store) do
    case store
         |> :khepri.put!(
           [:procs, :on_new_event],
           fn props -> 
             store 
             |> on_new_event(props) 
           end) do
      :ok ->
        on_new_event_filter =
          :khepri_evf.tree([store, :streams], %{on_actions: [:create]})

        store
        |> :khepri.register_trigger(
          :on_new_event_filter,
          on_new_event_filter,
          [:procs, :on_new_event]
        )
        :ok

      reason ->
        Logger.error("Failed to register filter. Reason: #{inspect(reason)}")
    end
  end

  def register_erl_emitter(store),
    do:
      :scarab_pubsub
      |> :func_registrations.register_emitter(store)
end
