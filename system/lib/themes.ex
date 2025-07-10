defmodule ExESDB.Themes do
  @moduledoc false
  alias BCUtils.ColorFuncs, as: CF

  def app(pid, msg),
    do: "[#{CF.black_on_blue()}#{inspect(pid)}#{CF.reset()}][APP] #{inspect(msg)}"

  def system(pid, msg),
    do: "[#{CF.black_on_magenta()}#{inspect(pid)}#{CF.reset()}][SYSTEM] #{inspect(msg)}"

  def store(pid, msg),
    do: "[#{CF.black_on_green()}#{inspect(pid)}#{CF.reset()}][STORE] #{inspect(msg)}"

  def cluster(pid, msg),
    do: "[#{CF.yellow_on_blue()}#{inspect(pid)}#{CF.reset()}][CLUSTER] #{inspect(msg)}"

  def cluster_coordinator(pid, msg),
    do: "[#{CF.white_on_blue()}#{inspect(pid)}#{CF.reset()}][CLUSTER_COORDINATOR] #{inspect(msg)}"

  def cluster_system(pid, msg),
    do:
      "[#{CF.bright_white_on_blue()}#{inspect(pid)}#{CF.reset()}][CLUSTER_SYSTEM] #{inspect(msg)}"

  def node_monitor(pid, msg),
    do: "[#{CF.yellow_on_magenta()}#{inspect(pid)}#{CF.reset()}][NODE_MONITOR] #{inspect(msg)}"

  def projector(pid, msg),
    do: "[#{CF.black_on_white()}#{inspect(pid)}#{CF.reset()}][PROJECTOR] #{inspect(msg)}"

  def monitor(pid, msg),
    do: "[#{CF.yellow_on_magenta()}#{inspect(pid)}#{CF.reset()}][MONITOR] #{inspect(msg)}"

  def emitter_pool(pid, msg),
    do: "[#{CF.yellow_on_black()}#{inspect(pid)}#{CF.reset()}][EMITTER_POOL] #{inspect(msg)}"

  def emitter_worker(pid, msg),
    do: "[#{CF.yellow_on_black()}#{inspect(pid)}#{CF.reset()}][EMITTER] #{inspect(msg)}"

  def persistent_emitter_worker(pid, msg),
    do: "[#{CF.yellow_on_black()}#{inspect(pid)}#{CF.reset()}][PERS.EMITTER] #{inspect(msg)}"

  def pubsub(pid, msg),
    do: "[#{CF.black_on_cyan()}#{inspect(pid)}#{CF.reset()}][PUBSUB] #{inspect(msg)}"

  ############ LEADER SYSTEM ##############
  def leader_system(pid, msg),
    do: "[#{CF.black_on_magenta()}#{inspect(pid)}#{CF.reset()}][LEADER_SYSTEM] #{inspect(msg)}"

  def leader_worker(pid, msg),
    do: "[#{CF.black_on_magenta()}#{inspect(pid)}#{CF.reset()}][LEADER_WORKER] #{inspect(msg)}"

  def leader_tracker(pid, msg),
    do: "[#{CF.black_on_magenta()}#{inspect(pid)}#{CF.reset()}][LEADER_TRACKER] #{inspect(msg)}"

  ######## SUBSCRIPTIONS ############
  def subscriptions(pid, msg),
    do: "[#{CF.black_on_white()}#{inspect(pid)}#{CF.reset()}][SUBSCRIPTIONS] #{inspect(msg)}"

  def subscriptions_reader(pid, msg),
    do:
      "[#{CF.black_on_white()}#{inspect(pid)}#{CF.reset()}][SUBSCRIPTIONS_READER] #{inspect(msg)}"

  def subscriptions_writer(pid, msg),
    do:
      "[#{CF.black_on_white()}#{inspect(pid)}#{CF.reset()}][SUBSCRIPTIONS_WRITER] #{inspect(msg)}"

  ########## STREAMS ###############
  def streams(pid, msg),
    do: "[#{CF.bright_yellow_on_blue()}#{inspect(pid)}#{CF.reset()}][STREAMS] #{inspect(msg)}"

  ############## STREAMS_READER ##############
  def streams_reader(pid, msg),
    do:
      "[#{CF.bright_yellow_on_blue()}#{inspect(pid)}#{CF.reset()}][STREAMS_READER] #{inspect(msg)}"

  def streams_reader_pool(pid, msg),
    do:
      "[#{CF.bright_yellow_on_blue()}#{inspect(pid)}#{CF.reset()}][STREAMS_READER_POOL] #{inspect(msg)}"

  def streams_reader_worker(pid, msg),
    do:
      "[#{CF.bright_yellow_on_blue()}#{inspect(pid)}#{CF.reset()}][STREAMS_READER_WORKER] #{inspect(msg)}"

  ############## STREAMS_WRITER ##############
  def streams_writer(pid, msg),
    do:
      "[#{CF.bright_yellow_on_black()}#{inspect(pid)}#{CF.reset()}][STREAMS_WRITER] #{inspect(msg)}"

  def streams_writer_pool(pid, msg),
    do:
      "[#{CF.bright_yellow_on_black()}#{inspect(pid)}#{CF.reset()}][STREAMS_WRITER_POOL] #{inspect(msg)}"

  def streams_writer_system(pid, msg),
    do:
      "[#{CF.bright_yellow_on_black()}#{inspect(pid)}#{CF.reset()}][STREAMS_WRITER_SYSTEM] #{inspect(msg)}"

  def streams_writer_worker(pid, msg),
    do:
      "[#{CF.bright_yellow_on_black()}#{inspect(pid)}#{CF.reset()}][STREAMS_WRITER_WORKER] #{inspect(msg)}"

  ########## GATEWAY ###############
  def gateway_worker(pid, msg),
    do:
      "[#{CF.bright_cyan_on_black()}#{inspect(pid)}#{CF.reset()}][GATEWAY_WORKER] #{inspect(msg)}"

  def gateway_supervisor(pid, msg),
    do:
      "[#{CF.bright_cyan_on_black()}#{inspect(pid)}#{CF.reset()}][GATEWAY_SUPERVISOR] #{inspect(msg)}"

  def gateway_api(pid, msg),
    do: "[#{CF.bright_cyan_on_black()}#{inspect(pid)}#{CF.reset()}][GATEWAY_API] #{inspect(msg)}"

  ################ SNAPSHOTS ################
  def snapshots_system(pid, msg),
    do: "[#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}][SNAPSHOTS_SYSTEM] #{inspect(msg)}"

  def snapshots_reader(pid, msg),
    do: "[#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}][SNAPSHOTS_READER] #{inspect(msg)}"

  def snapshots_reader_pool(pid, msg),
    do:
      "[#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}][SNAPSHOTS_READER_POOL] #{inspect(msg)}"

  def snapshots_reader_worker(pid, msg),
    do:
      "[#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}][SNAPSHOTS_READER_WORKER] #{inspect(msg)}"

  def snapshots_writer(pid, msg),
    do: "[#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}][SNAPSHOTS_WRITER] #{inspect(msg)}"

  def snapshots_writer_pool(pid, msg),
    do:
      "[#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}][SNAPSHOTS_WRITER_POOL] #{inspect(msg)}"

  def snapshots_writer_worker(pid, msg),
    do:
      "[#{CF.blue_on_yellow()}#{inspect(pid)}#{CF.reset()}][SNAPSHOTS_WRITER_WORKER] #{inspect(msg)}"
end
