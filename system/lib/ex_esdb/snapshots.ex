defmodule ExESDB.Snapshots do
  @moduledoc """
    Provides functions for working with snapshots
  """
  alias ExESDB.Schema.SnapshotRecord, as: SnapshotRecord

  @doc """
    Delete a snapshot of the current state of the event store.
  """
  @spec delete_snapshot(
          store :: any,
          source_uuid :: any
        ) :: :ok | {:error, any}
  def delete_snapshot(store, source_uuid) do
    case store
         |> :khepri.delete!([:snapshots, source_uuid]) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
    Read a snapshot of the current state of the event store.
  """
  @spec read_snapshot(
          store :: any,
          source_uuid :: any
        ) :: {:ok, SnapshotRecord.t()} | {:error, any}
  def read_snapshot(store, source_uuid) do
    case store
         |> :khepri.get!([:snapshots, source_uuid]) do
      {:ok, snapshot_record} -> {:ok, snapshot_record}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
    Record a snapshot of the current state of the event store.
  """
  @spec record_snapshot(
          store :: any,
          snapshot_record :: any
        ) :: :ok | {:error, any}
  def record_snapshot(store, %{source_uuid: source_uuid} = snapshot_record)
      when is_struct(snapshot_record, SnapshotRecord) do
    case store
         |> :khepri.put!([:snapshots, source_uuid], snapshot_record) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end
end
