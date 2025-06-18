defmodule ExESDB.Repl do
  @moduledoc """
    This module is to interact with the ExESDB.system, 
    running a store called "reg_gh" (Regulate Greenhouse)
    
  """
  alias ExESDB.Repl.EventGenerator, as: ESGen

  alias ExESDB.GatewayAPI, as: API

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

  def get_streams, do: API.get_streams(@store)

  def get_subscriptions, do: API.get_subscriptions(@store)

  @doc """
    Append events to a stream.
  """
  @spec append(
          stream :: atom(),
          nbr_of_events :: integer()
        ) :: {:ok, list(), integer()} | {:error, term()}
  def append(stream, nbr_of_events) do
    version = API.get_version!(@store, stream)

    events = ESGen.generate_events(version, nbr_of_events)

    case @store
         |> API.append_events(stream, version, events) do
      {:ok, new_version} ->
        {:ok, result} =
          @store
          |> API.get_events(stream, 1, new_version, :forward)

        {:ok, result, result |> Enum.count()}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def start_all_consumer() do
  end

  def start_stream_consumer(
        selector,
        start_from \\ 0,
        subscriber \\ nil,
        susciption_name \\ "transient"
      ) do
    opts = get_opts()
  end
end
