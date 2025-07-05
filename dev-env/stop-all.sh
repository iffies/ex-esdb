#!/bin/bash

set -e

echo "=== Stopping All ExESDB Clusters ==="

# Stop massive cluster (tier 3)
echo "Stopping massive cluster nodes..."
docker-compose \
  -f ex-esdb-cluster3.yaml \
  --profile cluster \
  -p cluster3 \
  down 2>/dev/null || true

# Stop extended cluster (tier 2)
echo "Stopping extended cluster nodes..."
docker-compose \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  down 2>/dev/null || true

# Stop core cluster (tier 1)
echo "Stopping core cluster nodes..."
docker-compose \
  -f ex-esdb-volumes.yaml \
  -f ex-esdb-network.yaml \
  -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  down 2>/dev/null || true

echo "✅ All ExESDB clusters stopped successfully!"
