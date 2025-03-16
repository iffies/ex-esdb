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
    data_dir: "data",
    store_id: :default_scarab_es,
    timeout: 10_000,
    db_type: :node
  ]

import_config "runtime.exs"
