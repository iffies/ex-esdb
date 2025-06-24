defmodule ExESDB.Emitters do
  @moduledoc """
    As part of the ExESDB.System, ExESDB.Emitters is responsible for managing the
    lifetime of the Emitter processes.
  """
  alias ExESDB.Topics, as: Topics

  defp build_filter(:by_stream, selector), do: :streams_filters.by_stream(selector)
  defp build_filter(:by_event_type, selector), do: :streams_filters.by_event_type(selector)
  defp build_filter(:by_event_pattern, selector), do: :streams_filters.by_event_pattern(selector)
  defp build_filter(:by_event_payload, selector), do: :streams_filters.by_event_payload(selector)

  defp partition(term) do
    partitions = System.schedulers_online()
    :erlang.phash2(term, (:rand.uniform(partitions) + 1) * 10)
  end

  def start_emitter(
        store,
        %{
          type: type,
          subscription_name: subscription_name,
          selector: selector,
          subscriber: subscriber
        },
        pool_size \\ 3
      ) do
    filter =
      type
      |> build_filter(selector)

    sub_topic = Topics.sub_topic(type, subscription_name, selector)

    args = {store, sub_topic, subscriber, pool_size, filter}

    DynamicSupervisor.start_child(
      {:via, PartitionSupervisor, {ExESDB.EmitterPools, partition(args)}},
      {ExESDB.EmitterPool, args}
    )
  end
end
