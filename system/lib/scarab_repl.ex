defmodule ScarabRepl do
  @moduledoc """
  This module is used to start a REPL with the Scarab system running.
  """
  alias Scarab.Config, as: ScarabConfig
  alias Scarab.EventStore, as: ScarabEventStore
  alias Scarab.Repl.EventGenerator, as: EventGenerator
  alias Scarab.System, as: ScarabSystem

  require Logger

  @scarab_app :scarab_app
  @store_id :sell_goods_at_pos
  @stream_name1 :order_stream1
  @stream_name2 :order_stream2
  @stream_name3 :order_stream3
  @stream_name4 :order_stream4
  @stream_name5 :order_stream5

  def store, do: @store_id
  def stream1, do: @stream_name1
  def stream2, do: @stream_name2
  def stream3, do: @stream_name3
  def stream4, do: @stream_name4
  def stream5, do: @stream_name5

  def get_config, do: ScarabConfig.fetch_env!(@scarab_app)
  def start, do: ScarabSystem.start(get_config())
  def get_streams, do: ScarabEventStore.get_streams(@store_id)

  def append_events do
    events =
      EventGenerator.generate_events(20)

    {:ok, expected_version} = ScarabEventStore.stream_version(@store_id, @stream_name1)

    {:ok, new_version} =
      @store_id
      |> ScarabEventStore.append_to_stream(@stream_name1, expected_version, events)

    {:ok, stream} = ScarabEventStore.read_stream_forward(@store_id, @stream_name1, 1, new_version)
    {:ok, stream, stream |> Enum.count()}
  end
end
