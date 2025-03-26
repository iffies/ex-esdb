import Config

alias ExESDB.EnVars, as: EnVars

config :logger, level: :info

config :ex_esdb, :khepri,
  data_dir: System.get_env(EnVars.data_dir()) || "/data",
  store_id: String.to_atom(System.get_env(EnVars.store_id()) || "ex_esdb"),
  timeout: String.to_integer(System.get_env(EnVars.timeout()) || "10000"),
  db_type: String.to_atom(System.get_env(EnVars.db_type()) || "single"),
  seeds_nodes: System.get_env(EnVars.seeds()) || []
