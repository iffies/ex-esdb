defmodule ExESDB.LeaderSystem do
  @moduledoc """
    This module supervises the Leader Subsystem.
  """
  use Supervisor
  require Logger
  alias ExESDB.Themes, as: Themes
  ############### PlUMBIng ############
  #
  def start_link(opts),
    do:
      Supervisor.start_link(
        __MODULE__,
        opts,
        name: __MODULE__
      )

  @impl true
  def init(config) do
    IO.puts("#{Themes.leader_system(self())} is UP!")
    Process.flag(:trap_exit, true)

    children = [
      {ExESDB.LeaderWorker, config},
      {ExESDB.SubscriptionsTracker, config}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
