defmodule ExESDB.Commanded.MapperTest do
  @moduledoc false

  use ExUnit.Case, async: true
  alias Commanded.EventStore.EventData
  alias ExESDB.Commanded.Mapper
  alias ExESDB.NewEvent
  require UUIDv7

  @doctest ExESDB.Commanded.Mapper

  describe "to_new_event/1" do

    @tag :mapper
    test "GIVEN: an event_data structure 
          WHEN: to_new_event is called 
          THEN: a new event is returned" do
      # GIVEN
      event_data = %EventData{
        event_type: "user_registered",
        data: %{email: "test@example.com"},
        metadata: nil,
        correlation_id: "corr-123",
        causation_id: "cause-456"
      }
      # WHEN
      result = Mapper.to_new_event(event_data)
      # THEN
      assert %NewEvent{} = result
      assert byte_size(result.event_id) == 36
      assert result.event_type == "user_registered"
      assert result.data_content_type == 1
      assert result.metadata_content_type == 1
      assert result.data == %{email: "test@example.com"}
      assert result.metadata.correlation_id == "corr-123"
      assert result.metadata.causation_id == "cause-456"
    end

    @tag :mapper
    test "GIVEN: an event_data structure
          WHEN: to_new_event is called
          THEN: the event_id is a UUIDv7 string" do
      original_uuid = UUIDv7.generate()
      # GIVEN
      event_data = %EventData{
        event_type: "tested",
        data: "data",
        correlation_id: "corr-123",
        causation_id: "cause-456"
      }
      # WHEN
      result = Mapper.to_new_event(event_data)
      # THEN
      refute result.event_id == original_uuid
      assert result.event_type == "tested"
      assert result.data == "data"
    end

    @tag :mapper
    test "GIVEN: an event_data structure
          WHEN: to_new_event is called
          THEN: the event_type is a string" do
      # GIVEN
      event_data = %EventData{
        event_type: "empty_event",
        data: nil,
        correlation_id: nil,
        causation_id: nil
      }
      # WHEN
      result = Mapper.to_new_event(event_data)
      # THEN
      assert result.event_type == "empty_event"
      assert result.data == nil
      assert result.metadata.correlation_id == nil
      assert result.metadata.causation_id == nil
    end
  end
end
