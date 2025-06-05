defmodule ExESDB.SubscriptionsHelper do
  @moduledoc """
   Provides functions for working with event store subscriptions.
  """

  def subscriptions_key(term),
    do: to_string(:erlang.phash2(term))
end
