defmodule ExESDB.Emitters do
  @moduledoc """
    As part of the ExESDB.System, ExESDB.Emitters is responsible for managing the
    lifetime of the Emitter processes.
  """

  alias ExESDB.Options

  # use DynamicSupervisor
  # @impl DynamicSupervisor
  # def init(_) do
  #   DynamicSupervisor.init(strategy: :one_for_one)
  # end

  defp build_filter(selector, :by_stream), do: :streams_filters.by_stream(selector)
  defp build_filter(selector, :by_event_type), do: :streams_filters.by_event_type(selector)
  defp build_filter(selector, :by_event_pattern), do: :streams_filters.by_event_pattern(selector)

  def start_emitter(
        store,
        %{
          type: type,
          subscription_name: subscription_name,
          selector: selector
        },
        pool_size \\ 3
      ) do
    pubsub = Options.pub_sub()

    filter =
      selector
      |> build_filter(type)

    sub_topic =
      if type == :by_event_pattern,
        do: subscription_name,
        else: selector

    DynamicSupervisor.start_child(
      {:via, PartitionSupervisor, {ExESDB.EmitterPools, self()}},
      {ExESDB.EmitterPool, {store, sub_topic, pubsub, pool_size, filter}}
    )
  end
end

defmodule ExESDB.EmitterPool do
  @moduledoc """
    As part of the ExESDB.System,
  """
  use Supervisor

  require Logger
  alias ExESDB.Themes, as: Themes

  def start_link({store, sub_topic, pubsub, pool_size, filter}) do
    Supervisor.start_link(__MODULE__, {store, sub_topic, pubsub, pool_size, filter},
      name: :"#{store}:#{sub_topic}_emitter_pool"
    )
  end

  @impl Supervisor
  def init({store, sub_topic, pubsub, pool_size, filter}) do
    scheduler_id = :erlang.system_info(:scheduler_id)

    emitters =
      store
      |> :emitter_group.setup_emitters(sub_topic, filter, pool_size)

    children =
      for emitter <- emitters do
        Supervisor.child_spec({ExESDB.EmitterWorker, {store, sub_topic, pubsub, emitter}},
          id: emitter
        )
      end

    IO.puts("#{Themes.emitter_pool(self())} is UP on scheduler #{inspect(scheduler_id)}")
    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule ExESDB.EmitterWorker do
  @moduledoc """
    As part of the ExESDB.System, 
    the EmitterWorker is responsible for managing the communication 
    between the Event Store and the PubSub mechanism.
  """
  use GenServer

  alias Phoenix.PubSub, as: PubSub

  require ExESDB.Themes, as: Themes

  require Logger

  defp emit(pub_sub, topic, event),
    do:
      pub_sub
      |> PubSub.broadcast(topic, {:event_emitted, event})

  @impl GenServer
  def init({store, id, pubsub}) do
    scheduler_id = :erlang.system_info(:scheduler_id)
    topic = :emitter_group.topic(store, id)
    :ok = :emitter_group.join(store, id, self())

    IO.puts(
      "#{Themes.emitter_worker(self())} for #{inspect(topic, pretty: true)} is UP on scheduler #{inspect(scheduler_id)}"
    )

    {:ok, pubsub}
  end

  def start_link({store, id, pubsub, emitter}),
    do:
      GenServer.start_link(
        __MODULE__,
        {store, id, pubsub},
        name: emitter
      )

  @impl true
  def handle_info({:broadcast, topic, event}, pubsub) do
    pubsub
    |> emit(topic, event)

    Logger.warning(
      "#{Themes.emitter_worker(self())} BROADCAST #{inspect(event, pretty: true)} => #{inspect(topic, pretty: true)}"
    )

    {:noreply, pubsub}
  end

  @impl true
  def handle_info({:forward_to_local, topic, event}, pubsub) do
    pubsub
    |> emit(topic, event)

    Logger.warning(
      "#{Themes.emitter_worker(self())} FORWARD_TO_LOCAL #{inspect(event, pretty: true)} => #{inspect(topic, pretty: true)}"
    )

    {:noreply, pubsub}
  end

  @impl true
  def handle_info(_, pubsub) do
    {:noreply, pubsub}
  end
end
