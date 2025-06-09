defmodule ExESDB.Themes do
  @moduledoc false
  alias BeamCampus.ColorFuncs, as: CF

  def app(pid),
    do: "APP [#{CF.black_on_blue()}#{inspect(pid)}#{CF.reset()}]"

  def system(pid),
    do: "SYSTEM [#{CF.black_on_magenta()}#{inspect(pid)}#{CF.reset()}]"

  def store(pid),
    do: "STORE [#{CF.black_on_green()}#{inspect(pid)}#{CF.reset()}]"

  def cluster(pid),
    do: "CLUSTER [#{CF.yellow_on_red()}#{inspect(pid)}#{CF.reset()}]"

  def projector(pid),
    do: "PROJECTOR [#{CF.black_on_white()}#{inspect(pid)}#{CF.reset()}]"

  def monitor(pid),
    do: "MONITOR [#{CF.yellow_on_magenta()}#{inspect(pid)}#{CF.reset()}]"

  def emitter_pool(pid),
    do: "EMITTER_POOL [#{CF.black_on_yellow()}#{inspect(pid)}#{CF.reset()}]"

  def emitter_worker(pid),
    do: "EMITTER_WORKER [#{CF.yellow_on_black()}#{inspect(pid)}#{CF.reset()}]"

  def pubsub(pid),
    do: "PUBSUB [#{CF.black_on_cyan()}#{inspect(pid)}#{CF.reset()}]"

  ############ LEADER SYSTEM ##############
  def leader_system(pid),
    do: "LEADER_SYSTEM [#{CF.black_on_magenta()}#{inspect(pid)}#{CF.reset()}]"

  def leader_worker(pid),
    do: "LEADER_WORKER [#{CF.black_on_magenta()}#{inspect(pid)}#{CF.reset()}]"

  def subscriptions_tracker(msg),
    do: "SUBSCRIPTIONS_TRACKER [#{CF.black_on_magenta()}#{inspect(msg)}#{CF.reset()}]"

  ######## SUBSCRIPTIONS ############
  def subscriptions(msg),
    do: "SUBSCRIPTIONS [#{CF.black_on_white()}#{inspect(msg)}#{CF.reset()}]"

  def subscriptions_reader(msg),
    do: "SUBSCRIPTIONS_READER [#{CF.black_on_white()}#{inspect(msg)}#{CF.reset()}]"

  def subscriptions_writer(msg),
    do: "SUBSCRIPTIONS_WRITER [#{CF.black_on_white()}#{inspect(msg)}#{CF.reset()}]"

  ########## STREAMS ###############
  def streams(msg),
    do: "STREAMS [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"

  def streams_reader(msg),
    do: "STREAMS_READER [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"

  def streams_writer(msg),
    do: "STREAMS_WRITER [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"

  ########## GATEWAY ###############
  def gateway(pid),
    do: "GATEWAY [#{CF.bright_cyan_on_black()}#{inspect(pid)}#{CF.reset()}]"
end
