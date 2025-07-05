#!/bin/bash

set -e

echo "=== Starting ExESDB Core Cluster (3 nodes: 0, 1, 2) ==="

# Clean up any existing core cluster
echo "Stopping any existing core cluster..."
docker-compose \
  -f ex-esdb-volumes.yaml \
  -f ex-esdb-network.yaml \
  -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  down 2>/dev/null || true

# Prepare data directories
echo "Preparing core cluster data directories..."
sudo mkdir -p /volume/caches
sudo rm -rf /volume/ex-esdb/data[0-2]
sudo mkdir -p \
  /volume/ex-esdb/data0 \
  /volume/ex-esdb/data1 \
  /volume/ex-esdb/data2

sudo chown "$USER" -R /volume/

# Start core cluster
echo "Starting core cluster (nodes 0, 1, 2)..."
docker-compose \
  -f ex-esdb-volumes.yaml \
  -f ex-esdb-network.yaml \
  -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  up \
  --remove-orphans \
  --build \
  -d

echo "✅ Core cluster started successfully!"
echo "   Nodes: ex-esdb0, ex-esdb1, ex-esdb2"
echo "   Total nodes: 3"
