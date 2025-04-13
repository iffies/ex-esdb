defmodule ExESDB.Themes do
  @moduledoc false
  alias BeamCampus.ColorFuncs, as: CF

  def app(pid),
    do: "ESDB_APP [#{CF.tui(:black, :blue)}#{inspect(pid)}#{CF.reset()}]"

  def system(pid),
    do: "ESDB_SYSTEM [#{CF.tui(:black, :magenta)}#{inspect(pid)}#{CF.reset()}]"

  def store(pid),
    do: "ESDB_STORE [#{CF.tui(:black, :green)}#{inspect(pid)}#{CF.reset()}]"

  def cluster(pid),
    do: "ESDB_CLUSTER [#{CF.tui(:yellow, :red)}#{inspect(pid)}#{CF.reset()}]"

  def projector(pid),
    do: "ESDB_PROJECTOR [#{CF.tui(:black, :white)}#{inspect(pid)}#{CF.reset()}]"

  def monitor(pid),
    do: "ESDB_MONITOR [#{CF.tui(:yellow, :magenta)}#{inspect(pid)}#{CF.reset()}]"

  def emitter(pid),
    do: "ESDB_EMITTER [#{CF.tui(:black, :yellow)}#{inspect(pid)}#{CF.reset()}]"
end
