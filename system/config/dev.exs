import Config

alias ExESDB.EnVars, as: EnVars

config :khepri,
  log_level: :info,
  logger: true

config :ra,
  log_level: :info,
  logger: true

config :logger, :console,
  format: "$time ($metadata) [$level] $message\n",
  metadata: [:mfa],
  level: :info

config :ex_esdb, :logger, level: :debug

config :ex_esdb, :khepri,
  data_dir: "tmp/reg_gh",
  store_id: :reg_gh,
  timeout: 10_000,
  db_type: :single,
  seed_nodes: [],
  pub_sub: :ex_esdb_pubsub
