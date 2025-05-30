# Getting Started with ExESDB

## Introduction

Event Sourcing with CQRS is a technique for building applications that are based on an immutable log of events, which makes it ideal for building concurrent, distributed systems.

Though it is gaining popularity, the number of options for storing these events is limited and require specialized services like Kurrent (aka Greg's EventStore) or AxonIQ.

One of the strong-points of the BEAM is, that it comes 'batteries included': there are BEAM-native libraries for many common tasks, like: storage, pub/sub, caching, logging, telemetry, etc.

`ExESDB` is an attempt to create a BEAM-native Event Store written in Elixir, building further upon the [Khepri](https://github.com/rabbitmq/khepri) library, which in turn builds upon the [Ra](https://github.com/rabbitmq/ra) library.

## Status

**This is a work in progress**

The project is in an early stage of development, and is not ready for production use.

Source code is available on [GitHub](https://github.com/beam-campus/ex-esdb).

## Installation

In your `mix.exs` file:

```elixir
def deps do
  [
    {:ex_esdb, "~> 0.0.8-alpha"}
  ]
end
```

## Configuration

1. in your `config/config.exs` file:

```elixir
config :ex_esdb, :khepri,
  # the directory where the khepri store will be created
  data_dir: "/data",
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

EX_ESDB_DATA_DIR="/data"
EX_ESDB_STORE_ID=ex_esdb_store
EX_ESDB_DB_TYPE=single
EX_ESDB_TIMEOUT=10000
EX_ESDB_SEED_NODES=""
EX_ESDB_PUB_SUB=ex_esdb_pub_sub

```

## Usage

```elixir
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    opts = ExESDB.Options.app_env()
    children = [
      {ExESDB.System, opts},
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
```
