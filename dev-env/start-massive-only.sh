#!/bin/bash

set -e

echo "=== Starting ExESDB Massive Tier Only (8 nodes: 20-27) ==="

# Clean up any existing massive cluster
echo "Stopping any existing massive cluster..."
docker-compose \
  -f ex-esdb-cluster3.yaml \
  --profile cluster \
  -p cluster3 \
  down 2>/dev/null || true

# Prepare massive cluster data directories
echo "Preparing massive cluster data directories..."
sudo mkdir -p /volume/caches
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

# Start massive nodes with staggered delays
echo "Starting massive cluster with staggered startup (5s delays)..."

# Start nodes in sequence with delays
for node_num in 20 21 22 23 24 25 26 27; do
  echo "  Starting ex-esdb${node_num}..."
  docker-compose \
    -f ex-esdb-cluster3.yaml \
    --profile cluster \
    -p cluster3 \
    up \
    $([ $node_num -eq 20 ] && echo "--remove-orphans --build") \
    -d \
    ex-esdb${node_num}
  
  # Wait before starting next node (except for the last one)
  if [ $node_num -ne 27 ]; then
    echo "  Waiting 5 seconds before starting next node..."
    sleep 5
  fi
done

echo "✅ Massive tier started successfully!"
echo "   Nodes: ex-esdb20, ex-esdb21, ex-esdb22, ex-esdb23, ex-esdb24, ex-esdb25, ex-esdb26, ex-esdb27"
echo "   Total nodes: 8"
echo "   Note: This is only the massive tier (requires core+extended clusters to be useful)"
