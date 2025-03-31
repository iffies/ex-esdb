defmodule ExESDB.Themes do
  @moduledoc false
  alias Colors, as: C

  def app(pid),
    do: "APP [#{C.black_on_blue()}#{inspect(pid)}#{C.reset()}]"

  def system(pid),
    do: "SYSTEM [#{C.yellow_on_black()}#{inspect(pid)}#{C.reset()}]"

  def store(pid),
    do: "STORE [#{C.black_on_green()}#{inspect(pid)}#{C.reset()}]"

  def cluster(pid),
    do: "CLUSTER [#{C.yellow_on_red()}#{inspect(pid)}#{C.reset()}]"

  def projector(pid),
    do: "PROJECTOR [#{C.black_on_white()}#{inspect(pid)}#{C.reset()}]"

  def monitor(pid),
    do: "MONITOR [#{C.yellow_on_magenta()}#{inspect(pid)}#{C.reset()}]"
end
