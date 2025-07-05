#!/bin/bash

set -e

echo "=== Starting ExESDB Extended Tier Only (2 nodes: 10, 11) ==="

# Clean up any existing extended cluster
echo "Stopping any existing extended cluster..."
docker-compose \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  down 2>/dev/null || true

# Prepare extended cluster data directories
echo "Preparing extended cluster data directories..."
sudo mkdir -p /volume/caches
sudo rm -rf /volume/ex-esdb/data1[0-1]
sudo mkdir -p \
  /volume/ex-esdb/data10 \
  /volume/ex-esdb/data11

sudo chown "$USER" -R /volume/

# Start extended nodes with staggered delays
echo "Starting extended cluster with staggered startup (5s delays)..."

# Start first extended node
echo "  Starting ex-esdb10..."
docker-compose \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  up \
  --remove-orphans \
  --build \
  -d \
  ex-esdb10

# Wait before starting second node
echo "  Waiting 5 seconds for ex-esdb10 to initialize..."
sleep 5

# Start second extended node
echo "  Starting ex-esdb11..."
docker-compose \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  up \
  -d \
  ex-esdb11

echo "✅ Extended tier started successfully!"
echo "   Nodes: ex-esdb10, ex-esdb11"
echo "   Total nodes: 2"
echo "   Note: This is only the extended tier (requires core cluster to be useful)"
