import Config

config :ex_unit,
  capture_log: false,
  assert_receive_timeout: 5_000,
  refute_receive_timeout: 1_000,
  exclude: [:skip],
  logger: true

config :ex_esdb, :khepri,
  data_dir: "tmp/reg_gh",
  store_id: :reg_gh,
  timeout: 1_000,
  db_type: :single,
  seed_nodes: []
