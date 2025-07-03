import Config

alias ExESDB.EnVars, as: EnVars

config :khepri,
  log_level: :info,
  logger: true

config :ra,
  log_level: :info,
  logger: true

config :logger, :console,
  format: "$time ($metadata) [$level] $message\n",
  metadata: [:mfa],
  level: :info

config :libcluster,
  topologies: [
    ex_esdb_cluster: [
      # The selected clustering strategy. Required.
      strategy: Elixir.Cluster.Strategy.Gossip,
      # Configuration for the selected strategy. Optional.
      config: [
        port: 45_892,
        # The IP address or hostname on which to listen for cluster connections.
        if_addr: "0.0.0.0",
        multicast_addr: "255.255.255.255",
        broadcast_only: true
      ]
    ]
  ]

config :ex_esdb, :logger, level: :debug

config :ex_esdb, :khepri,
  data_dir: "tmp/reg_gh",
  store_id: :reg_gh,
  timeout: 10_000,
  # Changed from :single to :cluster
  db_type: :cluster,
  pub_sub: :ex_esdb_pubsub
