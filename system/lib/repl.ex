defmodule ExESDB.Repl do
  @moduledoc """
    This module is to interact with the ExESDB.system, 
    running a store called "reg_gh" (Regulate Greenhouse)
    
  """
  alias ExESDB.Repl.EventGenerator, as: ESGen

  alias ExESDB.Gateway, as: ESGateway

  alias ExESDB.StreamsHelper, as: SHelper
  alias ExESDB.StreamsWriter, as: StrWriter

  alias ExESDB.SubscriptionsReader, as: SubsReader

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

  def get_streams, do: ESGateway.get_streams(@store)

  def get_subscriptions, do: SubsReader.get_subscriptions(@store)

  @doc """
    Append events to a stream.
  """
  @spec append(
          stream :: atom(),
          nbr_of_events :: integer()
        ) :: {:ok, list(), integer()} | {:error, term()}
  def append(stream, nbr_of_events) do
    version = SHelper.get_version!(@store, stream)

    events = ESGen.generate_events(version, nbr_of_events)

    case @store
         |> StrWriter.append_events(stream, version, events) do
      {:ok, new_version} ->
        {:ok, result} =
          @store
          |> ESGateway.get_events(stream, 1, new_version, :forward)

        {:ok, result, result |> Enum.count()}

      {:error, reason} ->
        {:error, reason}
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
