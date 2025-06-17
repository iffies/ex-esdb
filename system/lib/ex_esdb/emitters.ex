defmodule ExESDB.Emitters do
  @moduledoc """
    As part of the ExESDB.System, ExESDB.Emitters is responsible for managing the
    lifetime of the Emitter processes.
  """

  alias ExESDB.Options

  # use DynamicSupervisor
  # @impl DynamicSupervisor
  # def init(_) do
  #   DynamicSupervisor.init(strategy: :one_for_one)
  # end

  defp build_filter(selector, :by_stream), do: :streams_filters.by_stream(selector)
  defp build_filter(selector, :by_event_type), do: :streams_filters.by_event_type(selector)
  defp build_filter(selector, :by_event_pattern), do: :streams_filters.by_event_pattern(selector)

  def start_emitter(
        store,
        %{
          type: type,
          subscription_name: subscription_name,
          selector: selector
        },
        pool_size \\ 3
      ) do
    pubsub = Options.pub_sub()

    filter =
      selector
      |> build_filter(type)

    sub_topic =
      if type == :by_event_pattern,
        do: subscription_name,
        else: selector

    DynamicSupervisor.start_child(
      {:via, PartitionSupervisor, {ExESDB.EmitterPools, self()}},
      {ExESDB.EmitterPool, {store, sub_topic, pubsub, pool_size, filter}}
    )
  end
end
