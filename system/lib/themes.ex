defmodule ExESDB.Themes do
  @moduledoc false
  alias BeamCampus.ColorFuncs, as: CF

  def app(pid),
    do: "ESDB_APP [#{CF.black_on_blue()}#{inspect(pid)}#{CF.reset()}]"

  def system(pid),
    do: "ESDB_SYSTEM [#{CF.black_on_magenta()}#{inspect(pid)}#{CF.reset()}]"

  def store(pid),
    do: "ESDB_STORE [#{CF.black_on_green()}#{inspect(pid)}#{CF.reset()}]"

  def cluster(pid),
    do: "ESDB_CLUSTER [#{CF.yellow_on_red()}#{inspect(pid)}#{CF.reset()}]"

  def projector(pid),
    do: "ESDB_PROJECTOR [#{CF.black_on_white()}#{inspect(pid)}#{CF.reset()}]"

  def monitor(pid),
    do: "ESDB_MONITOR [#{CF.yellow_on_magenta()}#{inspect(pid)}#{CF.reset()}]"

  def emitter_pool(pid),
    do: "ESDB_EMITTER_POOL [#{CF.black_on_yellow()}#{inspect(pid)}#{CF.reset()}]"

  def emitter_worker(pid),
    do: "ESDB_EMITTER_WORKER [#{CF.yellow_on_black()}#{inspect(pid)}#{CF.reset()}]"

  def pubsub(pid),
    do: "ESDB_PUBSUB [#{CF.black_on_cyan()}#{inspect(pid)}#{CF.reset()}]"

  ######## SUBSCRIPTIONS ############
  def subscriptions(msg),
    do: "ESDB_SUBSCRIPTIONS [#{CF.black_on_white()}#{inspect(msg)}#{CF.reset()}]"

  def subscriptions_reader(msg),
    do: "ESDB_SUBSCRIPTIONS_READER [#{CF.black_on_white()}#{inspect(msg)}#{CF.reset()}]"

  def subscriptions_writer(msg),
    do: "ESDB_SUBSCRIPTIONS_WRITER [#{CF.black_on_white()}#{inspect(msg)}#{CF.reset()}]"

  ########## STREAMS ###############
  def streams(msg),
    do: "ESDB_STREAMS [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"

  def streams_reader(msg),
    do: "ESDB_STREAMS_READER [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"

  def streams_writer(msg),
    do: "ESDB_STREAMS_WRITER [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"
end
