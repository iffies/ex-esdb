defmodule Scarab.EventEmitter do
  @moduledoc false

  alias Phoenix.PubSub, as: PubSub

  require Logger

  def emit(store, event),
    do:
      :scarab_pubsub
      |> PubSub.broadcast(store, %{event: event})

  def emit!(store, event),
    do:
      :scarab_pubsub
      |> PubSub.broadcast!(store, %{event: event})
end
