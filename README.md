# ExESDB - A BEAM-native Event Store

ExESDB is a BEAM-native Event Store, built on top of the [khepri](https://github.com/rabbitmq/khepri) and [ra](https://github.com/rabbitmq/ra) subsystems.

## Motivation

One of the arguments for BEAM development is that it comes "batteries included". Be it caching, storage, pub/sub, observability etc... the Erlang ecosystem always has the option to avoid external dependencies.

For Event Sourcing use cases however, the Event Store is often a separate service.

This project is attempt at addressing this point, by building further upon the work of the `rabbitmq/khepri` and `rabbitmq/ra` subsystems.

## Contents

- [Getting Started](system/guides/getting_started.md)
