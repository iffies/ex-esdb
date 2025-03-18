import Config

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

config :scarab_es,
  khepri: [
    data_dir: "tmp/dev_dir",
    store_id: String.to_atom(System.get_env("SCARAB_STORE") || "default_scarab_es"),
    timeout: 10_000,
    db_type: :node
  ]
