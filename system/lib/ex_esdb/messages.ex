defmodule ScarabES.Messages do
  @moduledoc """
  Messages are based on official proto file
  """
  use Protobuf,
    from: Path.expand("../../priv/protos/scarab_es.proto", __DIR__),
    use_package_names: true
end
