import Config

alias ExESDB.EnVars, as: EnVars
import ExESDB.Options

config :logger, :console,
  format: "$time [$level] $message\n",
  metadata: [:mfa],
  level: :info,
  # Multiple filters to reduce noise from various components
  filters: [
    ra_noise: {ExESDB.LoggerFilters, :filter_ra},
    khepri_noise: {ExESDB.LoggerFilters, :filter_khepri},
    swarm_noise: {ExESDB.LoggerFilters, :filter_swarm},
    libcluster_noise: {ExESDB.LoggerFilters, :filter_libcluster}
  ]

config :ex_esdb, :khepri,
  data_dir: data_dir(),
  store_id: store_id(),
  timeout: timeout(),
  db_type: db_type(),
  pub_sub: pub_sub()

config :libcluster,
  topologies: [
    ex_esdb_cluster: [
      # The selected clustering strategy. Required.
      strategy: Cluster.Strategy.Gossip,
      # Configuration for the selected strategy. Optional.
      config: [
        port: 45_892,
        # The IP address or hostname on which to listen for cluster connections.
        if_addr: "0.0.0.0",
        # Use broadcast for cluster discovery
        multicast_addr: "255.255.255.255",
        broadcast_only: true
        # Shared secret for cluster security - read from environment at runtime
        #       secret: System.get_env("EX_ESDB_CLUSTER_SECRET") || "dev_cluster_secret"
      ]
    ]
  ]
