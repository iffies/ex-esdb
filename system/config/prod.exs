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

config :scarab_es,
  log_level: :information,
  khepri: [
    data_dir: "/data",
    store_id: :default_scarab_es,
    timeout: 10_000,
    db_type: :node
  ]
