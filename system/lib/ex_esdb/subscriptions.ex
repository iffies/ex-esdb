defmodule ExESDB.Subscriptions do
  @moduledoc """
   Provides functions for working with event store subscriptions.
  """

  use Supervisor

  ####### PLUMBING #######

  # def child_spec(opts), 
  #   do:
  #   %{
  #     id: __MODULE__,
  #     start: {__MODULE__, :start_link, [opts]},
  #     type: :worker,
  #     restart: :permanent,
  #     shutdown: 5000
  #   }
  #

  def start_link(opts),
    do:
      Supervisor.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  @impl true
  def init(opts) do
    children = [
      {ExESDB.SubscriptionsReader, opts},
      {ExESDB.SubscriptionsWriter, opts}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
