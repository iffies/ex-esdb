import Config

config :ex_unit,
  capture_log: true,
  assert_receive_timeout: 5_000,
  refute_receive_timeout: 1_000,
  exclude: [:skip]

config :test_app,
  scarab: [
    data_dir: "tmp/test_dir",
    store_id: :test_store,
    timeout: 1_000
  ]
