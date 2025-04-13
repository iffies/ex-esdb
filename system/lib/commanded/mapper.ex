defmodule ExESDB.Commanded.Mapper do
  @moduledoc """
    A mapper for Commanded to use ExESDB as the event store.
  """
  alias Commanded.EventStore.EventData, as: EventData
  alias Commanded.EventStore.RecordedEvent, as: RecordedEvent
  alias ExESDB.EventRecord, as: EventRecord
  alias ExESDB.NewEvent, as: NewEvent

  require UUIDv7

  @doc """
    Converts a Commanded EventData struct to an ExESDB NewEvent struct.
    
    Example:
    iex> event_data = %Commanded.EventStore.EventData{
    ...>   event_type: "OrderPlaced",
    ...>   data: %{order_id: "123", customer_id: "456"},
    ...>   metadata: %{correlation_id: "123", causation_id: "456"},
    ...>   correlation_id: "123",
    ...>   causation_id: "456"
    ...> }
    iex> event_id = UUIDv7.generate()
    iex> ExESDB.Mapper.to_new_event(event_data)
    %NewEvent{
      event_id: ^event_id,
      event_type: "OrderPlaced",
      data_content_type: 1,
      metadata_content_type: 1,
      data: %{order_id: "123", customer_id: "456"},
      metadata: %{
        correlation_id: "123",
        causation_id: "456"
        stream_version: 0
      }
    }
  """
  @spec to_new_event(EventData.t()) :: NewEvent.t()
  def to_new_event(event_data)
      when is_struct(event_data, EventData),
      do: %NewEvent{
        event_id: UUIDv7.generate(),
        event_type: event_data.event_type,
        data_content_type: 1,
        metadata_content_type: 1,
        data: event_data.data,
        metadata: %{
          correlation_id: event_data.correlation_id,
          causation_id: event_data.causation_id,
          stream_version: nil
        }
      }

  @doc """
    Converts an ExESDB EventRecord struct to a Commanded RecordedEvent struct.

    Example:
    iex> event_stream_id = "stream-1"
    ^event_stream_id
    iex> event_id = UUIDv7.generate()
    ^event_id
    iex> created = DateTime.utc_now()
    ^created
    iex> created_epoch = DateTime.to_unix(created, :millisecond)
    ^created_epoch
    iex> event_record = %EventRecord{
    ...>   event_stream_id: "stream-1",
    ...>   event_number: 1,
    ...>   event_id: ^event_id,
    ...>   event_type: "OrderPlaced",
    ...>   data_content_type: 1,
    ...>   metadata_content_type: 1,
    ...>   data: %{order_id: "123", customer_id: "456"},
    ...>   metadata: %{
    ...>     correlation_id: "123",
    ...>     causation_id: "456",
    ...>     stream_version: 0
    ...>   },
    ...>   created: ^created,
    ...>   created_epoch: ^created_epoch
    ...> }
    ^event_record
    iex> ExESDB.Mapper.to_recorded_event(event_record)
    %RecordedEvent{
      event_id: ^event_id,
      event_number: 1,
      event_type: "OrderPlaced",
      data: %{
        order_id: "123", 
        customer_id: "456"
      },
      metadata: %{
        correlation_id: "123",
        causation_id: "456",
        stream_version: 0
      },
      created_at: ^created,
      stream_id: "stream-1",
      stream_version: 0,
      correlation_id: "123",
      causation_id: "456"
    }
  """
  @spec to_recorded_event(EventRecord.t()) :: RecordedEvent.t()
  def to_recorded_event(
        %{
          metadata: %{
            stream_version: stream_version,
            correlation_id: correlation_id,
            causation_id: causation_id
          }
        } = event_record
      )
      when is_struct(event_record, EventRecord),
      do: %RecordedEvent{
        event_id: event_record.event_id,
        event_number: event_record.event_number,
        event_type: event_record.event_type,
        data: event_record.data,
        metadata: event_record.metadata,
        created_at: event_record.created,
        stream_id: event_record.event_stream_id,
        stream_version: stream_version,
        correlation_id: correlation_id,
        causation_id: causation_id
      }
end
