defmodule ExESDB.Commanded.Mapper do
  @moduledoc """
    A mapper for Commanded to use ExESDB as the event store.
  """
  alias Commanded.EventStore.EventData, as: EventData
  alias ExESDB.NewEvent, as: NewEvent
  require UUIDv7

  @spec to_new_event(EventData.t()) :: NewEvent.t()
  def to_new_event(event_data)
    when is_struct(event_data, EventData),
    do:
    %NewEvent{
      event_id: UUIDv7.generate(),
      event_type: event_data.event_type,
      data_content_type: 1,
      metadata_content_type: 1,
      data: event_data.data,
      metadata: %{
        correlation_id: event_data.correlation_id,
        causation_id: event_data.causation_id,
      }
    }

end
