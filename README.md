# ExESDB - A BEAM-native Event Store

ExESDB is a BEAM-native Event Store, built on top of the [khepri](https://github.com/rabbitmq/khepri) and [ra](https://github.com/rabbitmq/ra) subsystems.

## Motivation

One of the arguments for BEAM development is that it comes "batteries included". Be it caching, storage, pub/sub, observability etc... the Erlang ecosystem always has the option to avoid external dependencies.

For Event Sourcing use cases however, the Event Store is often a separate service.

This project is attempt at addressing this point, by building further upon the work of the `rabbitmq/khepri` and `rabbitmq/ra` subsystems.

## Installation

in your `mix.exs` file:

```elixir
def deps do
  [
    {:ex_esdb, "~> 0.1.0"}
  ]
end
```

## Configuration

1. in your `config/config.exs` file:

```elixir
config :ex_esdb, :khepri,
  # the directory where the khepri store will be created
  data_dir: "/tmp/ex_esdb",
  # the id of the khepri store
  store_id: "ex_esdb",
  # the type of database setup to use
  db_type: :single,
  # a global timeout in milliseconds
  timeout: 10_000,
```

2. from the ENVIRONMENT:

```bash

export EX_ESDB_DATA_DIR=/tmp/ex_esdb
export EX_ESDB_STORE_ID=ex_esdb
export EX_ESDB_DB_TYPE=single
export EX_ESDB_TIMEOUT=10000

```

## Usage

```elixir
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    ex_esdb_opts = ExESDB.Config.fetch_env!()
    children = [
      {ExESDB,System, ex_esdb_opts},
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
```
