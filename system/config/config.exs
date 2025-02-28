import Config

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :initial_call, :mfa]

import_config "#{Mix.env()}.exs"
