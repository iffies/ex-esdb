defmodule ExESDB.Repl.Producer do
  @moduledoc false
  use GenServer

  require Logger
  alias ExESDB.Repl.EventGenerator, as: ESGen
  alias ExESDB.StreamsHelper, as: SHelper
  alias ExESDB.StreamsWriter, as: StrWriter
  alias ExESDB.Themes, as: Themes

  defp append(store, stream, nbr_of_events) do
    version =
      store |> SHelper.get_version!(stream)

    events = ESGen.generate_events(version, nbr_of_events)

    {:ok, new_version} =
      store
      |> StrWriter.append_events(stream, version, events)

    IO.puts("Appended #{nbr_of_events} events to #{stream}. Now at version #{new_version}.")
  end

  @impl true
  def handle_info(:produce, state) do
    stream = Keyword.get(state, :stream)
    store = Keyword.get(state, :store)
    batch_size = Keyword.get(state, :batch_size, 1)

    store
    |> append(stream, batch_size)

    Process.send_after(self(), :produce, 2_000)
    {:noreply, state}
  end

  ############## PLUMBING ##############
  @impl true
  def init(args) do
    Process.send_after(self(), :produce, 2_000)
    {:ok, args}
  end

  def start_link(args) do
    greenhouse = Keyword.get(args, :stream, "greenhouse0")

    GenServer.start_link(
      __MODULE__,
      args,
      name: Module.concat(__MODULE__, greenhouse)
    )
  end

  def start_producer(args) do
    greenhouse =
      args
      |> Keyword.get(:stream, "greenhouse0")

    store =
      args
      |> Keyword.get(:store, :reg_gh)

    case start_link(args) do
      {:ok, pid} ->
        IO.puts("#{Themes.producer(pid)} for [#{inspect(store)}:#{greenhouse}] is UP!")
        pid

      {:error, {:already_started, pid}} ->
        IO.puts("#{Themes.producer(pid)} for [#{inspect(store)}:#{greenhouse}] is UP!")

        pid

      {:error, reason} ->
        raise "Failed to start producer. Reason: #{inspect(reason)}"
    end
  end
end
