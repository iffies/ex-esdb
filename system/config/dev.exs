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

config :scarab_app,
  scarab: [
    data_dir: "tmp/dev_dir",
    store_id: :sell_goods_at_pos,
    timeout: 10_000,
    db_type: :node
  ]

config :repl_app,
  scarab: [
    data_dir: "tmp/dev_dir",
    store_id: :sell_goods_at_pos,
    timeout: 10_000,
    db_type: :node
  ]
