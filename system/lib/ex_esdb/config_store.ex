defmodule ExESDB.ConfigStore do
  @moduledoc """
  A GenServer wrapper around a dedicated Khepri store for configuration data.
  This store is separate from data stores and optimized for metadata operations.
  """
  use GenServer

  require Logger
  alias ExESDB.Options, as: Options
  alias ExESDB.Themes, as: Themes

  defp start_khepri(opts) do
    timeout = opts[:timeout]
    data_dir = Path.join(opts[:data_dir], "config")
    :khepri.start(data_dir, :ex_esdb_config, timeout)
  end

  # Client API
  @doc """
  Get the current state of the config store.
  ## Returns

      - `{:ok, state}`  if successful.
      - `{:error, reason}` if unsuccessful.

  """
  def get_state,
    do:
      GenServer.call(
        __MODULE__,
        {:get_state}
      )

  ## CALLBACKS
  @impl true
  def handle_call({:get_state}, _from, state) do
    {:reply, {:ok, state}, state}
  end

  #### PLUMBING
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 10_000,
      type: :worker
    }
  end

  def start_link(opts),
    do:
      GenServer.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  # Server Callbacks
  @impl true
  def init(opts) do
    IO.puts("#{Themes.config_store(self(), "is UP.")}")
    Process.flag(:trap_exit, true)

    case start_khepri(opts) do
      {:ok, store} ->
        Logger.debug("Started config store: #{inspect(store)}")
        {:ok, [config: opts, store: store]}

      reason ->
        Logger.error("Failed to start config store khepri. reason: #{inspect(reason)}")
        {:error, [config: opts, store: nil]}
    end
  end
end
