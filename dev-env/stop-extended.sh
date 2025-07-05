#!/bin/bash

set -e

echo "=== Stopping ExESDB Extended Tier ==="

docker-compose \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  down

echo "✅ Extended tier stopped successfully!"
