defmodule ExESDB.StreamsWriter do
  @moduledoc """
    Provides functions for writing streams
  """

  use GenServer

  alias ExESDB.StreamsHelper, as: Helper

  ############ INTERNALS ############
  defp handle_transaction_result({:ok, {:commit, result}}), do: {:ok, result}
  defp handle_transaction_result({:ok, {:abort, reason}}), do: {:error, reason}
  defp handle_transaction_result({:error, reason}), do: {:error, reason}

  defp try_append_events(store, stream_id, expected_version, events) do
    current_version =
      store
      |> Helper.get_version!(stream_id)

    if current_version == expected_version do
      new_version =
        events
        |> Enum.reduce(
          current_version,
          fn event, version ->
            new_version = version + 1
            padded_version = Helper.pad_version(new_version, 6)

            now =
              DateTime.utc_now()

            created = now

            created_epoch =
              now
              |> DateTime.to_unix(:microsecond)

            recorded_event =
              event
              |> Helper.to_event_record(
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

      {:ok, new_version}
    else
      {:error, :wrong_expected_version}
    end
  end

  ########### API ############
  @doc """
    Append events to a stream.
  """
  @spec append_events(
          store :: atom(),
          stream_id :: any(),
          expected_version :: integer(),
          events :: list()
        ) :: {:ok, integer()} | {:error, term()}
  def append_events(store, stream_id, expected_version, events),
    do:
      GenServer.call(
        __MODULE__,
        {:append_events_tx, store, stream_id, expected_version, events}
      )

  ############ CALLBACKS ############
  @impl true
  def handle_call({:append_events_tx, store, stream_id, expected_version, events}, _from, state) do
    result =
      case store
           |> :khepri.transaction(fn ->
             store
             |> try_append_events(stream_id, expected_version, events)
           end)
           |> handle_transaction_result() do
        {:ok, new_version} ->
          {:ok, new_version}

        {:error, reason} ->
          {:error, reason}
      end

    {:reply, result, state}
  end

  ############ PLUMBING ############
  @impl true
  def init(opts) do
    {:ok, opts}
  end

  def child_spec(opts),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5000
    }

  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )
end
