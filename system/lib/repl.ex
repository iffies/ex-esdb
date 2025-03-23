defmodule ScarabES.Repl do
  @moduledoc """
  This module is used to start a REPL with the ScarabES.system running.
  """
  alias ScarabES.Config, as: ScarabConfig
  alias ScarabES.EventStore, as: ScarabEventStore
  alias ScarabES.Repl.EventGenerator, as: EventGenerator
  alias ScarabES.Repl.EventStreamMonitor, as: EventStreamMonitor

  # alias ScarabES.EventStreamReader, as: ESReader
  # alias ScarabES.EventStreamWriter, as: ESWriter

  alias ScarabES.EventStoreInfo, as: ESInfo

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

  def get_config, do: ScarabES.onfig.fetch_env!(@scarab_app)
  def get_streams, do: ESInfo.get_streams_raw!(@store)

  def start_monitor(store) do
    
    case  store 
      |> ScarabES.ventStore.get_state() do
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
      |> ScarabES.ventStore.stream_version(stream)
    {:ok, new_version} =
      @store
      |> ScarabES.ventStore.append_to_stream(stream, actual_version, [initialized])
    {:ok, result} =
      @store
      |> ScarabES.ventStore.read_stream_forward(stream, 1, new_version)
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
          |> ScarabES.ventStore.stream_version(stream)
        {:ok, new_version} =
          @store
          |> ScarabES.ventStore.append_to_stream(stream, actual_version, events)
        {:ok, result} =
          @store
          |> ScarabES.ventStore.read_stream_forward(stream, 1, new_version)
      {:ok, result, result |> Enum.count()}
    end
  end

  def all(stream) do
    case @store
         |> ScarabES.ventStore.stream_version(stream) do
      {:ok, 0} -> 
         nil

      {:ok, version} ->    
         {:ok, events} =              
            @store               
            |> ScarabES.ventStore.read_stream_forward(stream, 1, version)
            events
    end
  end

end
