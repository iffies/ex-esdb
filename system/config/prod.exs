import Config

config :khepri,
  log_level: :information,
  logger: true

config :ra,
  log_level: :information,
  logger: true

config :logger, :console,
  format: "$time ($metadata) [$level] $message\n",
  metadata: [:mfa],
  level: :debug

config :ex_esdb,
  logger: true,
  log_level: :debug

config :ex_esdb, :khepri,
  data_dir: "/data",
  store_id: :ex_esdb_store,
  timeout: 10_000,
  db_type: :single,
  seed_nodes: [],
  pub_sub: :ex_esdb_pub_sub
