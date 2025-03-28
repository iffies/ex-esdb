import Config

config :khepri,
  log_level: :warning,
  logger: true

config :ra,
  log_level: :warning,
  logger: true

config :logger, :console,
  format: "$time ($metadata) [$level] $message\n",
  metadata: [:mfa],
  level: :debug

config :ex_esdb, :logger, level: :debug

config :ex_esdb, :khepri,
  data_dir: "/data",
  store_id: :ex_store,
  timeout: 10_000,
  db_type: :single,
  seed_nodes: []
