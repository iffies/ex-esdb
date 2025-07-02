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

config :ex_esdb, :logger, level: :debug

# Partisan configuration for auto-discovery
config :partisan,
  # Peer service manager for membership
  peer_service_manager: :partisan_default_peer_service_manager,
  
  # Auto-connect to discovered peers
  auto_connect: true,
  
  # Membership strategy
  membership_strategy: :partisan_full_membership_strategy,
  
  # Don't use distributed Erlang
  connect_disterl: false,
  
  # Parallelism for better performance
  parallelism: 4,
  
  # Channels for communication
  channels: [
    :partisan_priority_channel,
    :partisan_causal_channel
  ],
  
  # Service discovery configuration
  peer_discovery: [
    enabled: true,
    # Using multicast for local network discovery
    module: :partisan_hyparview_xbot_peer_discovery,
    config: [
      # Service name for discovery
      service: "ex_esdb_server",
      # Discovery interval (5 seconds)
      interval: 5000,
      # Node basename for cluster
      node_basename: "ex_esdb"
    ]
  ]

config :ex_esdb, :khepri,
  data_dir: "tmp/reg_gh",
  store_id: :reg_gh,
  timeout: 10_000,
  db_type: :cluster,  # Changed from :single to :cluster
  seed_nodes: [],  # Will be populated by Partisan discovery
  pub_sub: :ex_esdb_pubsub
