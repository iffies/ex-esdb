defmodule ExESDB.StreamsWriterWorker do
  @moduledoc """
    Provides functions for writing streams
  """

  use GenServer

  alias ExESDB.StreamsHelper, as: Helper
  alias ExESDB.StreamsWriter, as: StreamsWriter
  alias ExESDB.Themes, as: Themes

  ############ INTERNALS ############
  defp handle_transaction_result({:ok, {:commit, result}}), do: {:ok, result}
  defp handle_transaction_result({:ok, {:abort, reason}}), do: {:error, reason}
  defp handle_transaction_result({:error, reason}), do: {:error, reason}

  defp try_append_events(store, stream_id, expected_version, events) do
    current_version =
      store
      |> Helper.get_version!(stream_id)

    if current_version == expected_version do
      new_version =
        events
        |> Enum.reduce(
          current_version,
          fn event, version ->
            new_version = version + 1
            padded_version = Helper.pad_version(new_version, 6)
            now = DateTime.utc_now()
            created = now

            created_epoch =
              now
              |> DateTime.to_unix(:microsecond)

            recorded_event =
              event
              |> Helper.to_event_record(
                stream_id,
                new_version,
                created,
                created_epoch
              )

            store
            |> :khepri.put!([:streams, stream_id, padded_version], recorded_event)

            new_version
          end
        )

      {:ok, new_version}
    else
      {:error, :wrong_expected_version}
    end
  end

  ############ CALLBACKS ############
  @impl true
  def handle_call({:append_events_tx, store, stream_id, expected_version, events}, _from, state) do
    result =
      case store
           |> :khepri.transaction(fn ->
             store
             |> try_append_events(stream_id, expected_version, events)
           end)
           |> handle_transaction_result() do
        {:ok, new_version} ->
          {:ok, new_version}

        {:error, reason} ->
          {:error, reason}
      end

    {:reply, result, state}
  end

  @impl true
  def handle_call({:append_events, store, stream_id, expected_version, events}, _from, state) do
    result =
      store
      |> try_append_events(stream_id, expected_version, events)

    {:reply, result, state}
  end

  ############# PLUMBING #############
  def child_spec({store, stream_id, partition}) do
    %{
      id: {StreamsWriter.worker_id(store, stream_id), partition},
      start: {__MODULE__, :start_link, [{store, stream_id, partition}]},
      type: :worker,
      restart: :permanent,
      shutdown: 5000
    }
  end

  def start_link({store, stream_id, partition}) do
    GenServer.start_link(
      __MODULE__,
      {store, stream_id, partition},
      name: {:global, StreamsWriter.worker_id(store, stream_id)}
    )
  end

  @impl true
  def init({store, stream_id, partition}) do
    Process.flag(:trap_exit, true)
    name = StreamsWriter.worker_id(store, stream_id)
    msg = "[#{inspect(name)}] is UP on partition #{inspect(partition)}, joining the cluster."
    IO.puts("#{Themes.streams_writer_worker(msg)}")
    Swarm.register_name(name, self())

    {:ok,
     %{
       worker_name: name,
       store: store,
       stream_id: stream_id,
       partition: partition,
       node: node()
     }}
  end

  @impl true
  def terminate(reason, %{worker_name: name}) do
    msg = "[#{inspect(name)}] is TERMINATED with reason #{inspect(reason)}, leaving the cluster."
    IO.puts("#{Themes.streams_writer_worker(msg)}")
    Swarm.unregister_name(name)
    :ok
  end

  @impl true
  def handle_info({:EXIT, _pid, reason}, %{worker_name: name} = state) do
    msg = "[#{inspect(name)}] is EXITING with reason #{inspect(reason)}, leaving the cluster."
    IO.puts("#{Themes.streams_writer_worker(msg)}")
    Swarm.unregister_name(name)
    {:noreply, state}
  end
end
