#!/bin/bash

set -e

echo "=== Stopping ExESDB Core Cluster ==="

docker-compose \
  -f ex-esdb-volumes.yaml \
  -f ex-esdb-network.yaml \
  -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  down

echo "✅ Core cluster stopped successfully!"
