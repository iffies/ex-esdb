import Config

config :scarab_es,
  khepri: [
    data_dir: System.get_env("SCARAB_DATA") || "/data",
    store_id: String.to_atom(System.get_env("SCARAB_STORE")) || :default_scarab_es,
    timeout: System.get_env("SCARAB_TIMEOUT") || 10_000,
    db_type: :node
  ]
