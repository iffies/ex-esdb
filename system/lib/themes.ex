defmodule ExESDB.Themes do
  @moduledoc false
  alias BeamCampus.ColorFuncs, as: CF

  def app(pid),
    do: "ExESDB APP [#{CF.black_on_blue()}#{inspect(pid)}#{CF.reset()}]"

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
    do: "EMITTER_POOL [#{CF.yellow_on_black()}#{inspect(pid)}#{CF.reset()}]"

  def emitter_worker(pid),
    do: "EMITTER_WORKER [#{CF.yellow_on_black()}#{inspect(pid)}#{CF.reset()}]"

  def persistent_emitter_worker(pid),
    do: "PERSISTENT_EMITTER_WORKER [#{CF.yellow_on_black()}#{inspect(pid)}#{CF.reset()}]"

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

  ############## STREAMS_READER ##############
  def streams_reader(msg),
    do: "STREAMS_READER [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"

  def streams_reader_pool(msg),
    do: "STREAMS_READER_POOL [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"

  def streams_reader_worker(msg),
    do: "STREAMS_READER_WORKER [#{CF.bright_yellow_on_blue()}#{inspect(msg)}#{CF.reset()}]"

  ############## STREAMS_WRITER ##############
  def streams_writer(msg),
    do: "STREAMS_WRITER [#{CF.bright_yellow_on_red()}#{inspect(msg)}#{CF.reset()}]"

  def streams_writer_pool(msg),
    do: "STREAMS_WRITER_POOL [#{CF.bright_yellow_on_red()}#{inspect(msg)}#{CF.reset()}]"

  def streams_writer_system(msg),
    do: "STREAMS_WRITER_SYSTEM [#{CF.bright_yellow_on_red()}#{inspect(msg)}#{CF.reset()}]"

  def streams_writer_worker(msg),
    do: "STREAMS_WRITER_WORKER [#{CF.bright_yellow_on_red()}#{inspect(msg)}#{CF.reset()}]"

  ########## GATEWAY ###############
  def gateway_worker(pid),
    do: "GATEWAY_WORKER [#{CF.bright_cyan_on_black()}#{inspect(pid)}#{CF.reset()}]"

  def gateway_supervisor(pid),
    do: "GATEWAY_SUPERVISOR [#{CF.bright_cyan_on_black()}#{inspect(pid)}#{CF.reset()}]"

  def gateway_api(pid),
    do: "GATEWAY_API [#{CF.bright_cyan_on_black()}#{inspect(pid)}#{CF.reset()}]"

  ################ OBSERVER ################
  def observer(pid),
    do: "OBSERVER [#{CF.bright_yellow_on_black()}#{inspect(pid)}#{CF.reset()}]"

  def observed(msg),
    do: "SEEN [#{CF.bright_green_on_black()}#{inspect(msg)}#{CF.reset()}]"

  ################ SUBSCRIBER ##############
  def subscriber(pid),
    do: "SUBSCRIBER [#{CF.bright_yellow_on_green()}#{inspect(pid)}#{CF.reset()}]"

  def subscription_received(subscription_name, msg),
    do:
      "RECEIVED [#{CF.bright_green_on_black()}#{inspect(subscription_name)} #{inspect(msg)}#{CF.reset()}]"

  ################ PRODUCER ################
  def producer(pid),
    do: "PRODUCER [#{CF.bright_green_on_black()}#{inspect(pid)}#{CF.reset()}]"

  def appended(msg),
    do: "APPENDED [#{CF.bright_yellow_on_black()}#{inspect(msg)}#{CF.reset()}]"

  ################ SNAPSHOTS ################
  def snapshots_system(pid),
    do: "SNAPSHOT_SYSTEM [#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}]"

  def snapshots_reader(pid),
    do: "SNAPSHOT_READER [#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}]"

  def snapshots_reader_pool(pid),
    do: "SNAPSHOT_READER_POOL [#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}]"

  def snapshots_reader_worker(pid),
    do: "SNAPSHOT_READER_WORKER [#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}]"

  def snapshots_writer(pid),
    do: "SNAPSHOT_WRITER [#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}]"

  def snapshots_writer_pool(pid),
    do: "SNAPSHOT_WRITER_POOL [#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}]"

  def snapshots_writer_worker(pid),
    do: "SNAPSHOT_WRITER_WORKER [#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}]"
end
