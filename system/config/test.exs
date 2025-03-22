import Config

config :ex_unit,
  capture_log: false,
  assert_receive_timeout: 5_000,
  refute_receive_timeout: 1_000,
  exclude: [:skip],
  logger: true

config :scarab_es,
  khepri: [
    data_dir: "tmp/reg_gh",
    store_id: :reg_gh,
    timeout: 1_000,
    db_type: :node
  ],
  ra: [
    name: :test_app
  ]
