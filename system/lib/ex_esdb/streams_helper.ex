defmodule ExESDB.StreamsHelper do
  @moduledoc """
    Provides helper functions for working with event store streams.
  """

  import ExESDB.Khepri.Conditions

  def calculate_versions(start_version, count, direction) do
    case direction do
      :forward -> start_version..(start_version + count - 1)
      :backward -> start_version..(start_version - count + 1)
    end
  end

  def stream_exists?(store, stream_id),
    do:
      store
      |> :khepri.exists([:streams, stream_id])

  def version_to_integer(padded_version) when is_binary(padded_version) do
    padded_version
    |> String.trim_leading("0")
    |> case do
      # Handle all-zero case
      "" -> 0
      num_str -> String.to_integer(num_str)
    end
  end

  def pad_version(version, length) when is_integer(version) and length > 0 do
    version
    |> Integer.to_string()
    |> String.pad_leading(length, "0")
  end

  @doc """
    Returns the version of the stream.
    ## Parameters
     - `store` is the name of the store.
     - `stream_id` is the name of the stream.

    ## Returns
     - `{:ok, version}`  if successful.
     - `{:error, reason}` if unsuccessful.
  """
  def get_version!(store, stream_id) do
    case store
         |> :khepri.count([
           :streams,
           stream_id,
           if_node_exists(exists: true)
         ]) do
      {:ok, version} -> version
      _ -> 0
    end
  end

  def to_event_record(
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
