# Scarab

Scarab is a reincarnation of rabbitmq/khepri, specialized to use khepri as a BEAM-native Event Store.

## Motivation

One of the arguments for BEAM development is that it comes "batteries included". Be it caching, storage, pub/sub, observability etc... the Erlang ecosystem always has the option to avoid external dependencies.

For Event Sourcing use cases however, the Event Store is often a separate service.

This project is attempt at addressing this point, by building further upon the work of the rabbitmq/khepri and rabbitmq/ra subsystems.

## Usage

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Scarab.System, Scarab.Config.fetch_env!(:my_app)}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```
