# Getting Started with ExESDB, a BEAM-native Event Store

## Installation

in your `mix.exs` file:

```elixir
def deps do
  [
    {:ex_esdb, "~> 0.0.2-alpha"}
  ]
end
```

## Configuration

1. in your `config/config.exs` file:

```elixir
config :ex_esdb, :khepri,
  # the directory where the khepri store will be created
  data_dir: "/ex_esdb/data",
  # the id of the khepri store.
  store_id: :ex_esdb_store,
  # the type of database setup to use
  db_type: :single,
  # a global timeout in milliseconds
  timeout: 10_000,
  # a list of seed nodes to connect to
  seed_nodes: [],
  # the name of the pub/sub module to use
  pub_sub: :ex_esdb_pub_sub

```

2. from the ENVIRONMENT:

```bash

export EX_ESDB_DATA_DIR=/ex_esdb/data
export EX_ESDB_STORE_ID=ex_esdb_store
export EX_ESDB_DB_TYPE=single
export EX_ESDB_TIMEOUT=10000
export EX_ESDB_SEED_NODES=
export EX_ESDB_PUB_SUB=ex_esdb_pub_sub

```

## Usage

```elixir
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    opts = ExESDB.Options.app_env()
    children = [
      {ExESDB,System, opts},
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
```
