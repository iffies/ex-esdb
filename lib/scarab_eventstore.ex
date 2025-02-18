defmodule Scarab.Eventstore do
  @moduledoc """
    Scarab.Eventstore is a GenServer that handles the persistence of events in Khepri.
  """
  use GenServer

  require Logger

  alias :khepri, as: Khepri
  alias Schema.ScarabInit, as: ScarabInit

  defp start_khepri(%ScarabInit{
         data_dir: data_dir,
         store_id: store_id,
         timeout: timeout
       }),
       do: Khepri.start(data_dir: data_dir, store_id: store_id, timeout: timeout)

  defp start_khepri(nil), do: Khepri.start()

  @impl true
  def init(opts) do
    config = Keyword.get(opts, :config)
    start_khepri(config)
    {:ok, opts}
  end

  #### PLUMBING
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start, [opts]},
      restart: :permanent,
      shutdown: 5000,
      type: :worker
    }
  end

  def start(opts) do
    case start_link(opts) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      _ -> raise "failed to start eventstore"
    end
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
end
