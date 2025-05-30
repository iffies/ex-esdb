import Config

alias ExESDB.EnVars, as: EnVars
import ExESDB.Options

config :ex_esdb, :khepri,
  data_dir: data_dir(),
  store_id: store_id(),
  timeout: timeout(),
  db_type: db_type(),
  seed_nodes: seed_nodes(),
  pub_sub: pub_sub()
