defmodule ExESDB.Emitter do
  @moduledoc false

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

  def register_emitter(store, pub_sub),
    do:
      pub_sub
      |> :func_registrations.register_emitter(store)
end
