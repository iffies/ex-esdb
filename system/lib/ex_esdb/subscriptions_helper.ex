defmodule ExESDB.SubscriptionsHelper do
  @moduledoc """
   Provides functions for working with event store subscriptions.
  """

  def subscriptions_key(type, selector, name),
    do: :erlang.phash2({type, selector, name})
end
