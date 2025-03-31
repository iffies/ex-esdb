defmodule ExESDB.Repl do
  @moduledoc """
    This module is to interact with the ExESDB.system, 
    running a store called "reg_gh" (Regulate Greenhouse)
    
  """
  alias ExESDB.Repl.EventGenerator, as: EventGenerator
  alias ExESDB.Repl.EventStreamMonitor, as: EventStreamMonitor

  alias ExESDB.EventStoreInfo, as: ESInfo

  require Logger

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

  def get_opts, do: ExESDB.Options.app_env()
  def get_streams, do: ESInfo.get_streams_raw(@store)

  def start_monitor do
    opts = get_opts()
    case EventStreamMonitor.start_link(opts) do
       {:ok, pid} -> IO.puts "Monitor started with pid #{inspect(pid)}"
       {:error, {:already_started, pid}} -> IO.puts "Monitor already started with pid #{inspect(pid)}"
       {:error, reason} -> raise "Failed to start monitor. Reason: #{inspect(reason)}"
    end
  end

  def initialize(stream) do
    initialized = EventGenerator.initialize()
    actual_version =
      @store
      |> ESInfo.get_version!(stream)
    {:ok, new_version} =
      @store
      |> ExESDB.EventStore.append_to_stream(stream, actual_version, [initialized])
    {:ok, result} =
      @store
      |> ExESDB.EventStore.read_stream_forward(stream, 1, new_version)
    {:ok, result, result |> Enum.count()}
  end

  def append(stream, nbr_of_events) do
    case all(stream) do
      nil ->
        stream
        |> initialize()
        stream
          |> append(nbr_of_events)
      _ ->
        events =
        EventGenerator.generate_events(nbr_of_events)

        actual_version =
          @store
          |> ESInfo.get_version!(stream)
        {:ok, new_version} =
          @store
          |> ExESDB.EventStore.append_to_stream(stream, actual_version, events)
        {:ok, result} =
          @store
          |> ExESDB.EventStore.read_stream_forward(stream, 1, new_version)
      {:ok, result, result |> Enum.count()}
    end
  end

  def all(stream) do
    case @store
         |> ESInfo.get_version!(stream) do
      0 ->
        nil

      version ->
         {:ok, events} =
            @store
            |> ExESDB.EventStore.read_stream_forward(stream, 1, version)
        events
    end
  end

  def start_system do
    opts = get_opts()
    case ExESDB.System.start_link(opts) do
      {:ok, pid} ->
        IO.puts "System started with pid #{inspect(pid)}"
        pid
      {:error, {:already_started, pid}} ->
        IO.puts "System already started with pid #{inspect(pid)}"
        pid
      {:error, reason} ->
        raise "Failed to start system. Reason: #{inspect(reason)}"
    end
  end

end
