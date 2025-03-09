defmodule ScarabRepl do
  @moduledoc """
  This module is used to start a REPL with the Scarab system running.
  """
  alias Scarab.Config, as: ScarabConfig
  alias Scarab.EventStore, as: ScarabEventStore
  alias Scarab.Repl.EventGenerator, as: EventGenerator
  alias Scarab.System, as: ScarabSystem

  alias Scarab.EventStreamReader, as: ESReader
  alias Scarab.EventStreamWriter, as: ESWriter

  alias Scarab.EventStoreInfo, as: ESInfo

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
  def get_streams, do: ESInfo.get_streams_raw!(@store_id)

  def append_events(stream, nbr_of_events) do
    events =
      EventGenerator.generate_events(nbr_of_events)

    {:ok, actual_version} =
      @store_id
      |> ScarabEventStore.stream_version(stream)

    {:ok, new_version} =
      @store_id
      |> ScarabEventStore.append_to_stream(stream, actual_version, events)

    {:ok, result} =
      @store_id
      |> ScarabEventStore.read_stream_forward(stream, 1, new_version)

    {:ok, result, result |> Enum.count()}
  end

  def read_all_events do
    {:ok, version} = ScarabEventStore.stream_version(@store_id, @stream_name1)
    {:ok, events} = ScarabEventStore.read_stream_forward(@store_id, @stream_name1, 1, version)
    events
  end
end
