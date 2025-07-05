#!/bin/bash

set -e

echo "=== Starting ExESDB Core Cluster with Staggered Startup (3 nodes: 0, 1, 2) ==="

# Clean up any existing core cluster
echo "Stopping any existing core cluster..."
docker-compose \
  -f ex-esdb-volumes.yaml \
  -f ex-esdb-network.yaml \
  -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  down 2>/dev/null || true

# Start core cluster with staggered delays
echo "Starting core cluster with staggered startup (5s delays)..."

# Start first node (seed node)
echo "  Starting ex-esdb0 (seed node)..."
docker-compose \
  -f ex-esdb-volumes.yaml \
  -f ex-esdb-network.yaml \
  -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  up \
  --remove-orphans \
  --build \
  -d \
  ex-esdb0

# Wait for seed node to fully initialize
echo "  Waiting 5 seconds for ex-esdb0 to initialize..."
sleep 5

# Start second node
echo "  Starting ex-esdb1..."
docker-compose \
  -f ex-esdb-volumes.yaml \
  -f ex-esdb-network.yaml \
  -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  up \
  -d \
  ex-esdb1

# Wait before starting third node
echo "  Waiting 5 seconds for ex-esdb1 to join cluster..."
sleep 5

# Start third node
echo "  Starting ex-esdb2..."
docker-compose \
  -f ex-esdb-volumes.yaml \
  -f ex-esdb-network.yaml \
  -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  up \
  -d \
  ex-esdb2

echo "✅ Core cluster started successfully with staggered startup!"
echo "   Nodes: ex-esdb0, ex-esdb1, ex-esdb2"
echo "   Total nodes: 3"
echo "   Startup sequence: ex-esdb0 → (5s) → ex-esdb1 → (5s) → ex-esdb2"
echo ""
echo "⏳ Waiting 10 seconds for cluster formation..."
sleep 10

echo "📊 Checking cluster status..."
docker-compose -p cluster ps
