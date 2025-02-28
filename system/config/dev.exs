import Config

config :khepri,
  log_level: :warning,
  logger: false

config :ra,
  log_level: :warning,
  logger: false

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:application, :module],
  level: :warning

config :node_app,
  scarab: [
    data_dir: "tmp/dev_dir",
    store_id: :sell_goods_at_pos,
    timeout: 1_000,
    db_type: :node
  ],
  ra: [
    name: :test_app
  ]

config :cluster_app,
  scarab: [
    data_dir: "tmp/scarab_data",
    store_id: :receive_goods_at_wh,
    timeout: 2_000,
    db_type: :cluster
  ]
