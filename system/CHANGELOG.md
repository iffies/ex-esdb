# Changelog

## version 0.0.16 (2025.06.26)

### Snapshots

#### Features

Snapshots Subsystem provides cluster wide support for reading and writing snapshots, using a key derived from the `source_uuid`, `stream_uuid` and `version` of the snapshot.

- `record_snapshot/5` function
- `delete_snapshot/4` function
- `read_snapshot/4` function
- `list_snapshots/3` function

#### Supported by Gateway API

It is advised to use `ExESDB.GatewayAPI` to access the Snapshots Subsystem.

## version 0.0.15 (2025.06.15)

### Subscriptions

#### Transient subscriptions

- `:by_stream`, `:by_event_type`, `:by_event_pattern`, `:by_event_payload`
- Events are forwarded to `Phoenix.PubSub` for now

#### Persistent subscriptions

- `:by_stream`, with support for replaying from a given version
- Events are forwarded to a specific subscriber process
- `ack_event/3` function is provided

#### "Follow-the-Leader"

Emitter processes are automatically started on the leader node,
when a new leader is elected.

#### Gateway API

- A cluster-wide gateway API is provided
- is an entry point for all the other modules
- provides basic High-Availability and Load-Balancing

## version 0.0.9-alpha (2025.05.04)

### Subscriptions

- `ExESDB.Subscriptions` module
- `func_registrations.exs` file
- emitter trigger in `khepri` now only uses the `erlang`-native :pg library (process groups)

#### Skeleton support for Commanded

- `ExESDB.Commanded.Adapter` module
- `ExESDB.Commanded.Mapper` module

## version 0.0.8-alpha

### 2025.04.13

- Added `ExESDB.EventStore.stream_forward/4` function
- Added `BeamCampus.ColorFuncs` module
- Added `ExESDB.Commanded.Adapter` module
- Refactored `ExESDB.EventStreamReader` and `ExESDB.EventStreamWriter` modules:
- Streams are now read and written using the `ExESDB.Streams` module
- Removed `ExESDB.EventStreamReader` module
- Removed `ExESDB.EventStreamWriter` module

## version 0.0.7-alpha

## version 0.0.1-alpha

### 2025.03.25

- Initial release
