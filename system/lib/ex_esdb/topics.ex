defmodule ExESDB.Topics do
  @moduledoc """
    A module to calculate topic identifiers
  """

  def sub_topic(:by_stream, "transient", selector), do: selector
  def sub_topic(:by_stream, subscription_name, _selector), do: "*#{subscription_name}"
  def sub_topic(:by_event_type, "transient", selector), do: selector
  def sub_topic(:by_event_pattern, subscription_name, _selector), do: subscription_name
  def sub_topic(:by_event_payload, subscription_name, _selector), do: subscription_name
end
