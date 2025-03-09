defmodule Scarab.EventStreamWriter do
  @moduledoc false

  alias Scarab.EventStreamReader, as: ESReader

  @retry_attempts 3
  @retry_delay 500
  defp handle_transaction_result({:ok, {:commit, result}}), do: {:ok, result}
  defp handle_transaction_result({:ok, {:abort, reason}}), do: {:error, reason}
  defp handle_transaction_result({:error, reason}), do: {:error, reason}

  def append_events_tx(store, stream_id, events) do
    store
    |> :khepri.transaction(fn ->
      actual_version =
        store
        |> ESReader.get_current_version!(stream_id)

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
        padded_version = Scarab.VersionFormatter.pad_version(new_version, 6)

        now =
          DateTime.utc_now()

        created =
          now
          |> DateTime.to_unix(:millisecond)

        created_epoch =
          now
          |> DateTime.to_unix(:nanosecond)

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
         %Scarab.NewEvent{} = new_event,
         stream_id,
         version,
         created,
         created_epoch
       ),
       do: %Scarab.EventRecord{
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
