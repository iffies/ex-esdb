import Config

alias ExESDB.EnVars, as: EnVars

config :khepri,
  log_level: :warning,
  logger: false

config :ra,
  log_level: :warning,
  logger: false

config :logger, :console,
  format: "$time ($metadata) [$level] $message\n",
  metadata: [:mfa],
  level: :debug

config :ex_esdb, :logger, level: :information

config :ex_esdb, :khepri,
  data_dir: "tmp/reg_gh",
  store_id: :reg_gh,
  timeout: 10_000,
  db_type: :single,
  seed_nodes: []
