#!/bin/bash

set -e

echo "=== Starting ExESDB Massive Cluster (13 nodes: 0, 1, 2, 10, 11, 20-27) ==="

# Start extended cluster first (which includes core)
echo "Starting extended cluster..."
./start-extended.sh

# Wait for extended cluster to be ready
echo "Waiting for extended cluster to be ready..."
sleep 15

# Prepare massive cluster data directories
echo "Preparing massive cluster data directories..."
sudo rm -rf /volume/ex-esdb/data2[0-7]
sudo mkdir -p \
  /volume/ex-esdb/data20 \
  /volume/ex-esdb/data21 \
  /volume/ex-esdb/data22 \
  /volume/ex-esdb/data23 \
  /volume/ex-esdb/data24 \
  /volume/ex-esdb/data25 \
  /volume/ex-esdb/data26 \
  /volume/ex-esdb/data27

sudo chown "$USER" -R /volume/

# Start massive nodes
echo "Starting massive cluster nodes (20-27)..."
docker-compose \
  -f ex-esdb-cluster3.yaml \
  --profile cluster \
  -p cluster3 \
  up \
  --remove-orphans \
  --build \
  -d

echo "✅ Massive cluster started successfully!"
echo "   Core nodes: ex-esdb0, ex-esdb1, ex-esdb2"
echo "   Extended nodes: ex-esdb10, ex-esdb11"
echo "   Massive nodes: ex-esdb20, ex-esdb21, ex-esdb22, ex-esdb23, ex-esdb24, ex-esdb25, ex-esdb26, ex-esdb27"
echo "   Total nodes: 13"
