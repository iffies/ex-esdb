defmodule Scarab.Repl.EventGenerator do
  @moduledoc false

  require UUID

  @initialized_v1 "initialized:v1"
  @item_added_v1 "item_added:v1"
  @item_removed_v1 "item_removed:v1"
  @money_received_v1 "money_received:v1"
  @change_returned_v1 "change_returned:v1"

  @event_types [
    @initialized_v1,
    @item_added_v1,
    @item_removed_v1,
    @money_received_v1,
    @change_returned_v1
  ]

  @customers [
    "John",
    "Paul",
    "George",
    "Ringo",
    "Pete",
    "Scott",
    "Adam",
    "Eddie",
    "Fred",
    "Wilma",
    "Jane",
    "Betty",
    "Helen",
    "Mary",
    "Susan"
  ]

  @items [
    "Apple",
    "Banana",
    "Orange",
    "Grape",
    "Pear",
    "Strawberry",
    "Blueberry",
    "Pineapple",
    "Watermelon"
  ]

  @sizes [
    "Small",
    "Medium",
    "Large",
    "Extra Large",
    "Huge"
  ]
  # Example content type values
  @content_types [1, 2, 3]

  defp random_customer, do: Enum.random(@customers)
  defp random_item, do: Enum.random(@items)
  defp random_size, do: Enum.random(@sizes)
  defp random_quantity, do: :rand.uniform(100)
  defp random_money, do: :rand.uniform(100)

  def generate_events(count)
      when is_integer(count) and count > 0,
      do:
        1..count
        |> Enum.map(&generate_event/1)

  defp generate_event(_) do
    event_id = generate_uuid()
    event_type = Enum.random(@event_types)

    %Scarab.NewEvent{
      event_id: event_id,
      event_type: Enum.random(@event_types),
      data_content_type: Enum.random(@content_types),
      metadata_content_type: Enum.random(@content_types),
      data: generate_random_data(event_type),
      metadata: generate_optional_metadata()
    }
  end

  defp generate_uuid do
    UUID.uuid4()
  end

  defp generate_random_data(@initialized_v1),
    do: %{
      customer: random_customer()
    }

  defp generate_random_data(@item_added_v1),
    do: %{
      customer: random_customer(),
      item: random_item(),
      size: random_size(),
      quantity: random_quantity(),
      price: random_money()
    }

  defp generate_random_data(@item_removed_v1),
    do: %{
      customer: random_customer(),
      item: random_item(),
      size: random_size(),
      quantity: random_quantity(),
      price: random_money()
    }

  defp generate_random_data(@money_received_v1),
    do: %{
      customer: random_customer(),
      amount: random_money(),
      currency: "USD"
    }

  defp generate_random_data(@change_returned_v1),
    do: %{
      customer: random_customer(),
      amount: random_money(),
      currency: "USD"
    }

  defp generate_random_bytes(length) do
    :crypto.strong_rand_bytes(length)
    |> Base.encode64()
  end

  defp generate_optional_metadata do
    # 70% chance of metadata
    if :rand.uniform() < 0.7 do
      generate_random_bytes(8)
    end
  end
end
