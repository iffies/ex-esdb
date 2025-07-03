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
      strategy: Elixir.Cluster.Strategy.Epmd,
      # Configuration for the selected strategy. Optional.
      config: [
        hosts: [
          :"ex-esdb@127.0.0.1"
        ]
      ],
      connect: {:net_kernel, :connect_node, []},
      disconnect: {:erlang, :disconnect_node, []},
      list_nodes: {:erlang, :nodes, [:connected]}
    ]
  ]

config :ex_esdb, :logger, level: :debug

config :ex_esdb, :khepri,
  data_dir: "tmp/reg_gh",
  store_id: :reg_gh,
  timeout: 10_000,
  # Changed from :single to :cluster
  db_type: :cluster,
  # Will be populated by Partisan discovery
  seed_nodes: [],
  pub_sub: :ex_esdb_pubsub
