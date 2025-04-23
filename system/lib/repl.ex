defmodule ExESDB.Repl do
  @moduledoc """
    This module is to interact with the ExESDB.system, 
    running a store called "reg_gh" (Regulate Greenhouse)
    
  """
  alias ExESDB.Repl.EventGenerator, as: ESGen
  alias ExESDB.Repl.EventStreamMonitor, as: ESMonitor

  alias ExESDB.StoreInfo, as: ESInfo
  alias ExESDB.Streams, as: ESStreams
  alias ExESDB.Subscriptions, as: ESSubscriptions
  alias ExESDB.System, as: ESSystem

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

  def get_streams,
    do: ESInfo.get_streams_raw(@store)

  def get_subscriptions, do: ESSubscriptions.all(@store)

  def start_monitor do
    opts = get_opts()

    case ESMonitor.start_link(opts) do
      {:ok, pid} ->
        IO.puts("Monitor started with pid #{inspect(pid)}")

      {:error, {:already_started, pid}} ->
        IO.puts("Monitor already started with pid #{inspect(pid)}")

      {:error, reason} ->
        raise "Failed to start monitor. Reason: #{inspect(reason)}"
    end
  end

  @doc """
    Append events to a stream.
  """
  @spec append(
          stream :: atom(),
          nbr_of_events :: integer()
        ) :: {:ok, list(), integer()} | {:error, term()}
  def append(stream, nbr_of_events) do
    version = ESInfo.get_version!(@store, stream)

    events = ESGen.generate_events(version, nbr_of_events)

    case @store
         |> ESStreams.append_events(stream, version, events) do
      {:ok, new_version} ->
        {:ok, result} =
          @store
          |> ESStreams.stream_forward(stream, 1, new_version)

        {:ok, result, result |> Enum.count()}

      {:error, reason} ->
        {:error, reason}
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
          |> ESStreams.stream_forward(stream, 1, version)

        events
    end
  end

  def start_system do
    opts = get_opts()

    case ESSystem.start_link(opts) do
      {:ok, pid} ->
        IO.puts("System started with pid #{inspect(pid)}")
        pid

      {:error, {:already_started, pid}} ->
        IO.puts("System already started with pid #{inspect(pid)}")
        pid

      {:error, reason} ->
        raise "Failed to start system. Reason: #{inspect(reason)}"
    end
  end
end
