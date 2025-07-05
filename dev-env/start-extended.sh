#!/bin/bash

set -e

echo "=== Starting ExESDB Extended Cluster (5 nodes: 0, 1, 2, 10, 11) ==="

# Start core cluster first
echo "Starting core cluster..."
./start-core.sh

# Wait for core cluster to be ready
echo "Waiting for core cluster to be ready..."
sleep 10

# Prepare extended cluster data directories
echo "Preparing extended cluster data directories..."
sudo rm -rf /volume/ex-esdb/data1[0-1]
sudo mkdir -p \
  /volume/ex-esdb/data10 \
  /volume/ex-esdb/data11

sudo chown "$USER" -R /volume/

# Start extended nodes
echo "Starting extended cluster nodes (10, 11)..."
docker-compose \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  up \
  --remove-orphans \
  --build \
  -d

echo "✅ Extended cluster started successfully!"
echo "   Core nodes: ex-esdb0, ex-esdb1, ex-esdb2"
echo "   Extended nodes: ex-esdb10, ex-esdb11"
echo "   Total nodes: 5"
