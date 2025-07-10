defmodule ExESDB.ConfigSystem do
  @moduledoc """
  The ExESDB Configuration SubSystem.
  """
  use Supervisor

  require Logger
  alias ExESDB.Themes, as: Themes

  def start_link(opts),
    do: Supervisor.start_link(__MODULE__, opts, name: __MODULE__)

  @impl true
  def init(opts) do
    children = [
      {ExESDB.ConfigStore, opts},
      {PartitionSupervisor, child_spec: DynamicSupervisor, name: ExESDB.ConfigWriters},
      {PartitionSupervisor, child_spec: DynamicSupervisor, name: ExESDB.ConfigReaders},
      {ExESDB.ConfigTracker, opts}
    ]

    ret = Supervisor.init(children, strategy: :one_for_one)
    IO.puts("#{Themes.config_system(self(), "is UP.")}")
    ret
  end

  @doc """
  Returns the key for a store configuration as a Khepri Path.
  ## Examples
      iex> ExESDB.ConfigSystem.path(:my_store)
      [:stores, :config, :my_store]
  """
  @spec path(store_id :: atom()) :: list()
  def path(store_id) when is_atom(store_id) do
    [:stores, :config, store_id]
  end
end
