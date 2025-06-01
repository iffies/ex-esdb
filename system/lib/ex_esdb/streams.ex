defmodule ExESDB.Streams do
  @moduledoc """
    Provides functions for working with streams
  """
  alias ExESDB.StoreInfo, as: ESInfo

  defp handle_transaction_result({:ok, {:commit, result}}), do: {:ok, result}
  defp handle_transaction_result({:ok, {:abort, reason}}), do: {:error, reason}
  defp handle_transaction_result({:error, reason}), do: {:error, reason}

  @spec stream_events(
          store :: atom(),
          stream_id :: any(),
          start_version :: integer(),
          count :: integer()
        ) :: {:ok, Enumerable.t()} | {:error, term()}
  def stream_events(store, stream_id, start_version, count) do
    event_stream =
      start_version..(start_version + count - 1)
      |> Stream.map(fn version ->
        padded_version = ExESDB.VersionFormatter.pad_version(version, 6)

        store
        |> :khepri.get!([:streams, stream_id, padded_version])
      end)
      |> Stream.reject(&is_nil/1)

    {:ok, event_stream}
  end

  def get_streams(store) do
    store
    |> :khepri.get!([:streams])
    |> Enum.reduce([], fn {stream_id, _stream}, acc -> stream_id ++ acc end)
  end

  def stream_exists?(store, stream_id) do
    store
    |> :khepri.exists([:streams, stream_id])
  end

  @doc """
    Read events from a stream, in a forward direction.
  """
  @spec stream_forward(
          store :: atom(),
          stream_id :: any(),
          start_version :: integer(),
          count :: integer()
        ) :: {:ok, list()} | {:error, term()}
  def stream_forward(store, stream_id, start_version, count) do
    try do
      case store
           |> stream_exists?(stream_id) do
        true ->
          store
          |> stream_events(stream_id, start_version, count)

        false ->
          {:error, :stream_not_found}
      end
    rescue
      e -> {:error, e}
    end
  end

  @doc """
    Append events to a stream using a transaction.
  """
  @spec append_events_tx(
          store :: atom(),
          stream_id :: any(),
          events :: list()
        ) :: {:ok, integer()} | {:error, term()}
  def append_events_tx(store, stream_id, events) do
    case store
         |> :khepri.transaction(fn ->
           actual_version =
             store
             |> ESInfo.get_version!(stream_id)

           store
           |> append_events(stream_id, actual_version, events)
         end)
         |> handle_transaction_result() do
      {:ok, new_version} ->
        {:ok, new_version}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
    Append events to a stream.
  """
  @spec append_events(
          store :: atom(),
          stream_id :: any(),
          current_version :: integer(),
          events :: list()
        ) :: {:ok, integer()} | {:error, term()}
  def append_events(store, stream_id, current_version, events) do
    next_version =
      events
      |> Enum.reduce(
        current_version,
        fn event, version ->
          new_version = version + 1
          padded_version = ExESDB.VersionFormatter.pad_version(new_version, 6)

          now =
            DateTime.utc_now()

          created = now

          created_epoch =
            now
            |> DateTime.to_unix(:microsecond)

          recorded_event =
            event
            |> to_event_record(
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

    {:ok, next_version}
  end

  defp to_event_record(
         %ExESDB.NewEvent{} = new_event,
         stream_id,
         version,
         created,
         created_epoch
       ),
       do: %ExESDB.EventRecord{
         event_stream_id: stream_id,
         event_number: version,
         event_id: new_event.event_id,
         event_type: new_event.event_type,
         data_content_type: new_event.data_content_type,
         metadata_content_type: new_event.metadata_content_type,
         data: new_event.data,
         metadata: new_event.metadata,
         created: created,
         created_epoch: created_epoch
       }
end
