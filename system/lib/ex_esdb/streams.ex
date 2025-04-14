defmodule ExESDB.Streams do
  @moduledoc """
    Provides functions for working with streams
  """
  defp handle_transaction_result({:ok, {:commit, result}}), do: {:ok, result}
  defp handle_transaction_result({:ok, {:abort, reason}}), do: {:error, reason}
  defp handle_transaction_result({:error, reason}), do: {:error, reason}

  def read_events(store, stream_id, start_version, count) do
    start_version..(start_version + count - 1)
    |> Enum.map(fn version ->
      padded_version = ExESDB.VersionFormatter.pad_version(version, 6)

      store
      |> :khepri.get!([:streams, stream_id, padded_version])
    end)
    |> Enum.reject(&is_nil/1)
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

  def stream_forward(store, stream_id, start_version, count) do
    try do
      case store
           |> stream_exists?(stream_id) do
        true ->
          store
          |> read_events(stream_id, start_version, count)

        false ->
          {:error, :stream_not_found}
      end
    rescue
      e -> {:error, e}
    end
  end

  def append_events_tx(store, stream_id, events) do
    store
    |> :khepri.transaction(fn ->
      actual_version =
        store
        |> ESInfo.get_version!(stream_id)

      store
      |> append_events(stream_id, events, actual_version)
    end)
    |> handle_transaction_result()
  end

  def append_events(store, stream_id, events, current_version) do
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
