import Config

config :khepri,
  log_level: :information,
  logger: true

config :ra,
  log_level: :information,
  logger: true

config :logger, :console,
  format: "$time ($metadata) [$level] $message\n",
  metadata: [:mfa],
  level: :debug

config :ex_esdb,
  logger: true,
  log_level: :debug

config :ex_esdb, :khepri,
  data_dir: "/data",
  store_id: :reg_gh,
  timeout: 2_000,
  db_type: :cluster,
  seed_nodes: [],
  pub_sub: :ex_esdb_pubsub

config :libcluster,
  topologies: [
    ex_esdb: [
      strategy: Cluster.Strategy.Gossip,
      config: [
        port: 45892,
        if_addr: "0.0.0.0",
        multicast_if: "192.168.0.1",
        multicast_addr: "233.252.1.32",
        multicast_ttl: 1,
        secret: "T0pS3cr3t"
      ]
    ]
  ]
