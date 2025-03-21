defmodule ScarabRepl do
  @moduledoc """
  This module is used to start a REPL with the Scarab system running.
  """
  alias Scarab.Config, as: ScarabConfig
  alias Scarab.EventStore, as: ScarabEventStore
  alias Scarab.Repl.EventGenerator, as: EventGenerator
  alias Scarab.Repl.EventStreamMonitor, as: EventStreamMonitor

  # alias Scarab.EventStreamReader, as: ESReader
  # alias Scarab.EventStreamWriter, as: ESWriter

  alias Scarab.EventStoreInfo, as: ESInfo

  require Logger

  @scarab_app :scarab_es
  @store :reg_gh
  @greenhouse1 :greenhouse1
  @greenhouse2 :greenhouse2
  @greenhouse3 :greenhouse3
  @greenhouse4 :greenhouse4
  @greenhouse5 :greenhouse5

  def store, do: @store
  def stream1, do: @greenhouse1
  def stream2, do: @greenhouse2
  def stream3, do: @greenhouse3
  def stream4, do: @greenhouse4
  def stream5, do: @greenhouse5

  def get_config, do: ScarabConfig.fetch_env!(@scarab_app)
  def get_streams, do: ESInfo.get_streams_raw!(@store)

  def start_monitor(store) do
    
    case  store 
      |> ScarabEventStore.get_state() do
        {:ok, [config: config, store: _]} -> 
          IO.puts "Starting monitor for #{inspect(config, pretty: true)}"
          {:ok, _pid} = EventStreamMonitor.start_link(config)
     
        {:error, reason} -> raise "Failed to get state. Reason: #{inspect(reason)}"
    end
  end

  def initialize(stream) do
    initialized = EventGenerator.initialize()
    {:ok, actual_version} =
      @store
      |> ScarabEventStore.stream_version(stream)
    {:ok, new_version} =
      @store
      |> ScarabEventStore.append_to_stream(stream, actual_version, [initialized])
    {:ok, result} =
      @store
      |> ScarabEventStore.read_stream_forward(stream, 1, new_version)
    {:ok, result, result |> Enum.count()}
  end


  def append(stream, nbr_of_events) do
    case stream |> all() do
      nil -> 
        stream 
        |> initialize()
        stream
          |> append(nbr_of_events)
      _ ->
        events =
        EventGenerator.generate_events(nbr_of_events)

        {:ok, actual_version} =
          @store
          |> ScarabEventStore.stream_version(stream)
        {:ok, new_version} =
          @store
          |> ScarabEventStore.append_to_stream(stream, actual_version, events)
        {:ok, result} =
          @store
          |> ScarabEventStore.read_stream_forward(stream, 1, new_version)
      {:ok, result, result |> Enum.count()}
    end
  end

  def all(stream) do
    case @store
         |> ScarabEventStore.stream_version(stream) do
      {:ok, 0} -> 
         nil

      {:ok, version} ->    
         {:ok, events} =              
            @store               
            |> ScarabEventStore.read_stream_forward(stream, 1, version)
            events
    end
  end

end
