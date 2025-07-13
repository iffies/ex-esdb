import Config

config :ex_unit,
  capture_log: false,
  assert_receive_timeout: 5_000,
  refute_receive_timeout: 1_000,
  exclude: [:skip],
  logger: true

# Configure minimal logging for tests
config :logger, :console,
  format: {ExESDB.LogFormatter, :format},
  level: :warning

# Use minimal visual mode for tests (no emojis, clean output)
config :ex_esdb, :visual_mode, :minimal

config :ex_esdb, :khepri,
  data_dir: "tmp/ex_esdb_store",
  store_id: :ex_test_store,
  timeout: 1_000,
  db_type: :single,
  seed_nodes: [],
  pub_sub: :ex_esdb_pub_sub
