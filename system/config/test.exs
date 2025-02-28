import Config

config :ex_unit,
  capture_log: false,
  assert_receive_timeout: 5_000,
  refute_receive_timeout: 1_000,
  exclude: [:skip],
  logger: true

config :node_app,
  scarab: [
    data_dir: "tmp/scarab_data",
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
