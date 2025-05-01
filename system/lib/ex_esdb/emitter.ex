defmodule ExESDB.Emitter do
  @moduledoc """
    As part of the ExESDB.System,
  """
  use Supervisor

  require Logger
  require ExESDB.Themes, as: Themes

  def start_link(opts) do
    pubsub = Keyword.fetch!(opts, :pub_sub)
    pool_size = Keyword.get(opts, :pool_size, 3)
    store = Keyword.fetch!(opts, :store_id)

    Supervisor.start_link(__MODULE__, {store, pubsub, pool_size},
      name: :"#{store}_ex_esdb_emitter_supervisor"
    )
  end

  def register_emitter(store, stream_uuid) do
    :func_registrations.register_emitter(store, stream_uuid)
  end

  @impl Supervisor
  def init({store, pubsub, pool_size}) do
    Logger.warning("#{Themes.emitter(self())} is UP")
    register_emitter(store, :all)

    [_ | groups] =
      for number <- 1..pool_size do
        :"#{store}_ex_esdb_emitter_#{number}"
      end

    groups = [:"#{store}_ex_esdb_emitter" | groups]
    :persistent_term.put("#{store}_ex_esdb_emitters", List.to_tuple(groups))

    children =
      for group <- groups do
        Supervisor.child_spec({ExESDB.EmitterWorker, {store, pubsub, group}}, id: group)
      end

    Logger.warning("
      Starting Children: \n#{inspect(children)}
      ")

    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule ExESDB.EmitterWorker do
  @moduledoc """
    As part of the ExESDB.System, 
    the Emitter is responsible for managing the communication 
    between the Event Store and the PubSub mechanism.
  """
  use GenServer

  alias Phoenix.PubSub, as: PubSub

  require ExESDB.Themes, as: Themes

  require Logger

  def emit(store, pub_sub, event),
    do:
      pub_sub
      |> PubSub.broadcast("#{store}", {:event_emitted, event})

  @impl GenServer
  def init({store, pubsub}) do
    Logger.warning("#{Themes.emitter_worker(self())} is UP")
    :ok = pg_join("#{store}_ex_esdb_emitters")
    {:ok, pubsub}
  end

  def start_link({store, pubsub, group}),
    do:
      GenServer.start_link(
        __MODULE__,
        {store, pubsub},
        name: group
      )

  @impl true
  def handle_info({:broadcast, topic, event}, pubsub) do
    pubsub
    |> PubSub.broadcast(topic, {:event_emitted, event})

    Logger.warning("#{Themes.emitter(self())} received event #{inspect(event)}")
    {:noreply, pubsub}
  end

  @impl true
  def handle_info({:forward_to_local, topic, event}, pubsub) do
    pubsub
    |> Phoenix.PubSub.local_broadcast(topic, event)

    {:noreply, pubsub}
  end

  @impl true
  def handle_info(_, pubsub) do
    {:noreply, pubsub}
  end

  if Code.ensure_loaded?(:pg) do
    defp pg_join(group) do
      :ok = :pg.join(Phoenix.PubSub, group, self())
    end
  else
    defp pg_join(group) do
      namespace = {:phx, group}
      :ok = :pg2.create(namespace)
      :ok = :pg2.join(namespace, self())
      :ok
    end
  end
end
