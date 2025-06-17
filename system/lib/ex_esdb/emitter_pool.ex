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
