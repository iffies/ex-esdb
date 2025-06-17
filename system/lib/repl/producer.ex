defmodule ExESDB.Repl.Producer do
  @moduledoc false
  use GenServer

  require Logger
  alias ExESDB.Repl.EventGenerator, as: ESGen
  alias ExESDB.StreamsHelper, as: SHelper
  alias ExESDB.StreamsWriter, as: StrWriter
  alias ExESDB.Themes, as: Themes

  @impl true
  def handle_info(:produce, state) do
    stream = stream(state)
    store = store(state)
    batch_size = batch_size(state)
    period = period(state)

    store
    |> append(stream, batch_size)

    produce(period)
    {:noreply, state}
  end

  ############## PLUMBING ##############
  @impl true
  def init(args) do
    period = period(args)
    produce(period)
    {:ok, args}
  end

  def start_link(args) do
    stream = stream(args)

    GenServer.start_link(
      __MODULE__,
      args,
      name: Module.concat(__MODULE__, stream)
    )
  end

  @doc """
    Starts a producer process for testing purposes.
    ## Parameters
      * `store`: The store to use (atom, default: `:reg_gh`).
      * `stream`: The id of the stream to append events to (string, default: `greenhouse0`).
      * `batch_size`: The number of events to append in a single batch (integer, default: `1`).
      * `period`: The time in milliseconds between appending events (integer, default: `2000`).
  """
  @spec start_producer(keyword()) :: pid()
  def start_producer(args) do
    stream = stream(args)
    store = store(args)

    case start_link(args) do
      {:ok, pid} ->
        IO.puts("#{Themes.producer(pid)} for [#{inspect(store)}:#{stream}] is UP!")
        pid

      {:error, {:already_started, pid}} ->
        IO.puts("#{Themes.producer(pid)} for [#{inspect(store)}:#{stream}] is already UP!")

        pid

      {:error, reason} ->
        raise "Failed to start producer. Reason: #{inspect(reason)}"
    end
  end

  defp store(args), do: Keyword.get(args, :store, :reg_gh)
  defp stream(args), do: Keyword.get(args, :stream, "greenhouse0")
  defp batch_size(args), do: Keyword.get(args, :batch_size, 1)
  defp period(args), do: Keyword.get(args, :period, 2_000)
  defp produce(period), do: Process.send_after(self(), :produce, period)

  defp append(store, stream, nbr_of_events) do
    version =
      store |> SHelper.get_version!(stream)

    events = ESGen.generate_events(version, nbr_of_events)

    {:ok, new_version} =
      store
      |> StrWriter.append_events(stream, version, events)

    IO.puts("
             Appended #{nbr_of_events} event(s) to #{inspect(store)}:#{stream}. 
             Now at version #{new_version}.
            ")
  end
end
