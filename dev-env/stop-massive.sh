#!/bin/bash

set -e

echo "=== Stopping ExESDB Massive Tier ==="

docker-compose \
  -f ex-esdb-cluster3.yaml \
  --profile cluster \
  -p cluster3 \
  down

echo "✅ Massive tier stopped successfully!"
