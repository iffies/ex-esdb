# Changelog

## version 0.0.9-alpha

### 2025.05.04

#### Added support for Subscriptions

- `ExESDB.Subscriptions` module
- `func_registrations.exs` file
- emitter trigger in `khepri` now only uses the `erlang`-native :pg library (process groups)

#### Added support for Commanded

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
