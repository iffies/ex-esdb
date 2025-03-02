defmodule ScarabRepl do
  @moduledoc """
  This module is used to start a REPL with the Scarab system running.
  """
  alias Scarab.Config, as: ScarabConfig
  alias Scarab.EventStore, as: ScarabEventStore
  alias Scarab.NewEvent, as: NewEvent
  alias Scarab.Repl.EventGenerator, as: EventGenerator
  alias Scarab.System, as: ScarabSystem

  require Logger

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

  def get_config, do: ScarabConfig.fetch_env!(:dev_app)
  def start, do: ScarabSystem.start(get_config())
  def get_event_store_state, do: ScarabEventStore.get_state(@store_id)

  def append_events do
    events =
      EventGenerator.generate_events(10)

    {:ok, expected_version} = ScarabEventStore.stream_version(@store_id, @stream_name1)
    Logger.info("Expected version: #{inspect(expected_version)}")

    {:ok, new_version} =
      ScarabEventStore.append_to_stream(@store_id, @stream_name1, expected_version, events)

    Logger.info("New version: #{inspect(new_version)}")
    ScarabEventStore.read_stream_forward(@store_id, @stream_name1, 0, 10)
  end
end
