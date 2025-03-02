defmodule Scarab.Repl.EventGenerator do
  require UUID

  @event_types ["clicked:v1", "viewed:v1", "purchased:v1", "logged_in:v1", "logged_out:v1"]
  # Example content type values
  @content_types [1, 2, 3]

  def generate_events(count)
      when is_integer(count) and count > 0,
      do:
        1..count
        |> Enum.map(&generate_event/1)

  defp generate_event(_) do
    %Scarab.NewEvent{
      event_id: generate_uuid(),
      event_type: Enum.random(@event_types),
      data_content_type: Enum.random(@content_types),
      metadata_content_type: Enum.random(@content_types),
      data: generate_random_bytes(16),
      metadata: generate_optional_metadata()
    }
  end

  defp generate_uuid do
    UUID.uuid4()
    |> String.replace("-", "")
    |> Base.decode16!(case: :lower)
  end

  defp generate_random_bytes(length) do
    :crypto.strong_rand_bytes(length)
  end

  defp generate_optional_metadata do
    # 70% chance of metadata
    if :rand.uniform() < 0.7 do
      generate_random_bytes(8)
    end
  end
end
