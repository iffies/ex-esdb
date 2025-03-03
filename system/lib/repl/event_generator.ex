defmodule Scarab.Repl.EventGenerator do
  require UUID

  @event_types ["clicked:v1", "viewed:v1", "purchased:v1", "logged_in:v1", "logged_out:v1"]
  # Example content type values
  @content_types [1, 2, 3]

  @engines ["engine1", "engine2", "engine3"]

  def generate_events(count)
      when is_integer(count) and count > 0,
      do:
        1..count
        |> Enum.map(&generate_event/1)

  defp generate_event(_) do
    event_id = generate_uuid()

    %Scarab.NewEvent{
      event_id: event_id,
      event_type: Enum.random(@event_types),
      data_content_type: Enum.random(@content_types),
      metadata_content_type: Enum.random(@content_types),
      data: generate_random_data(),
      metadata: generate_optional_metadata()
    }
  end

  defp generate_uuid do
    UUID.uuid4()
  end

  defp generate_random_data do
    "{
    engine: #{Enum.random(@engines)},
    throttle: #{:random.uniform(100)},
    }"
  end

  defp generate_random_bytes(length) do
    :crypto.strong_rand_bytes(length) |> Base.encode64()
  end

  defp generate_optional_metadata do
    # 70% chance of metadata
    if :rand.uniform() < 0.7 do
      generate_random_bytes(8)
    end
  end
end
