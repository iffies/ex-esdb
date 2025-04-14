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

  def register_pg_emitter(store, pub_sub),
    do:
      store
      |> :func_registrations.register_pg_emitter(pub_sub)
end
