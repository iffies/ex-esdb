# Visual Logging in ExESDB

ExESDB features a rich visual logging system that preserves the library's distinctive personality while providing full configurability for different environments.

## Overview

The visual logging system converts traditional `IO.puts` calls to structured `Logger` calls while preserving:
- 🎨 Colorful themed prefixes for each component
- 🏆 Meaningful emojis as visual cues
- 📊 Hierarchical indentation for related events
- ➡️ Arrow indicators for important state changes

## Visual Elements

### Emojis and Their Meanings
- 🏆 Leader/winner status
- 🚀 Activation/startup
- 📊 Store/data
- 📝 Managing/documenting
- ✅ Success/joined
- ❌ Failure/left
- 🔄 Change/transition
- ⚠️ Warning
- 🔴 Previous/old state
- 🟢 New/current state
- 📞 Following/client mode
- 🔻 Going down/terminating
- ⚙️ Processing/working
- 🥈 Follower/non-leader node

### Component Prefixes
Each component has a clear bracketed prefix format:
- `[STORE CLUSTER #PID<0.123.0>]` - Cluster operations
- `[LEADER WORKER #PID<0.456.0>]` - Leader responsibilities  
- `[GATEWAY WORKER #PID<0.789.0>]` - API gateway operations
- `[EMITTER POOL #PID<0.234.0>]` - Event emission
- And more components following the same pattern

## Configuration

### Development (Full Visual Mode)
```elixir
# config/dev.exs
config :logger, :console,
  format: {ExESDB.LogFormatter, :format},
  metadata: [:component, :pid, :indent, :arrow]

config :ex_esdb, :visual_mode, :full
```

Output example:
```
[STORE CLUSTER #PID<0.685.0>] ==> 🚀 ACTIVATING LEADERSHIP RESPONSIBILITIES
  🏆 Node: :node1@host
  📊 Store: :ex_esdb_store
  📝 Managing 3 active subscriptions
```

### Staging (Standard Mode)
```elixir
# config/staging.exs
config :logger, :console,
  format: {ExESDB.LogFormatter, :format},
  metadata: [:component, :pid]

config :ex_esdb, :visual_mode, :standard
```

Output example:
```
[12:34:56] [info] [STORE CLUSTER #PID<0.685.0>] ==> 🚀 ACTIVATING LEADERSHIP RESPONSIBILITIES
[12:34:56] [info]   🏆 Node: :node1@host
```

### Production (Minimal Mode)
```elixir
# config/prod.exs
config :logger, :console,
  format: {ExESDB.LogFormatter, :format},
  metadata: [:component]

config :ex_esdb, :visual_mode, :minimal
```

Output example:
```
[12:34:56] [info] [STORE CLUSTER] ACTIVATING LEADERSHIP RESPONSIBILITIES
[12:34:56] [info] [STORE CLUSTER] Node: :node1@host
```

### Testing (Suppressed)
```elixir
# config/test.exs
config :logger, level: :warning
config :ex_esdb, :visual_mode, :standard
```

## Usage in Code

When logging, include appropriate metadata:

```elixir
# Main event with arrow
Logger.info("🚀 ACTIVATING LEADERSHIP RESPONSIBILITIES",
  component: :leader_worker,
  pid: self(),
  arrow: true
)

# Indented sub-item
Logger.info("📝 Managing 3 active subscriptions",
  component: :leader_worker,
  pid: self(),
  indent: 1
)

# Warning with emojis
Logger.warning("⚠️⚠️ Failed to get store members",
  component: :cluster,
  pid: self()
)
```

## Custom Logger Formatter

The `ExESDB.LogFormatter` module provides three formatting modes:

1. **`:full`** - Complete visual experience with colors and themed prefixes
2. **`:standard`** - Clean output with timestamps, emojis but no colors
3. **`:minimal`** - Production-ready minimal output without emojis

The formatter automatically:
- Applies component-specific bracketed prefixes in full mode
- Handles hierarchical indentation
- Adds arrow indicators for main events
- Strips emojis in minimal mode for clean production logs

## Benefits

1. **Preserves Library Personality**: The rich visual design that makes ExESDB distinctive is maintained
2. **Fully Configurable**: Different modes for different environments
3. **Structured Logging**: Metadata enables filtering and analysis
4. **Production Ready**: Can be configured for standard logging in production
5. **Test Friendly**: Can be suppressed or redirected for testing
6. **Backwards Compatible**: Developers who love the visual output can keep it

## Migration from IO.puts

The conversion from `IO.puts` to `Logger` preserves all visual elements:

```elixir
# Before
IO.puts("#{Themes.store_cluster(self(), "🚀 ACTIVATING")}")

# After
Logger.info("🚀 ACTIVATING",
  component: :store_cluster,
  pid: self(),
  arrow: true
)
```

The visual output remains identical in full mode, while gaining configurability.