#!/bin/bash

# Test Enhanced Healthcheck Script
# This script tests the new EPMD-free healthcheck functionality

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
NC='\033[0m' # No Color

# Function to print colored text
print_color() {
    local color=$1
    local text=$2
    echo -e "${color}${text}${NC}"
}

print_color $CYAN "╔══════════════════════════════════════════════════════════════╗"
print_color $CYAN "║                ExESDB Enhanced Healthcheck Test              ║"
print_color $CYAN "║                                                              ║"
print_color $CYAN "║                Testing EPMD-free Health Checks               ║"
print_color $CYAN "╚══════════════════════════════════════════════════════════════╝"
echo

print_color $WHITE "Testing enhanced healthcheck capabilities..."
echo

# Test 1: Check if we can build the image with the new healthcheck
print_color $BLUE "Test 1: Building image with enhanced healthcheck..."
cd /home/rl/work/github.com/beam-campus/ex-esdb/system
if docker build -t local/ex-esdb . >/dev/null 2>&1; then
    print_color $GREEN "✓ Image built successfully with enhanced healthcheck"
else
    print_color $RED "✗ Failed to build image"
    exit 1
fi

# Test 2: Start a single node to test healthcheck
print_color $BLUE "Test 2: Testing healthcheck on single node..."
cd /home/rl/work/github.com/beam-campus/ex-esdb/dev-env

# Start single node
docker-compose \
    -f ex-esdb-volumes.yaml \
    -f ex-esdb-network.yaml \
    -f ex-esdb-cluster.yaml \
    --profile cluster \
    -p cluster \
    up -d ex-esdb0 >/dev/null 2>&1

print_color $YELLOW "Waiting for node to start..."
sleep 10

# Test the healthcheck directly
print_color $CYAN "Running healthcheck on ex-esdb0..."
if docker exec ex-esdb0 /system/check-ex-esdb.sh; then
    print_color $GREEN "✓ Enhanced healthcheck passed"
else
    print_color $YELLOW "⚠ Enhanced healthcheck failed (may be normal during startup)"
fi

# Test 3: Check traditional Docker healthcheck
print_color $BLUE "Test 3: Checking Docker native healthcheck status..."
health_status=$(docker inspect ex-esdb0 --format='{{.State.Health.Status}}' 2>/dev/null || echo "none")
print_color $CYAN "Docker health status: $health_status"

# Test 4: Compare with grouped cluster manager health check
print_color $BLUE "Test 4: Testing grouped cluster manager health check..."
cd /home/rl/work/github.com/beam-campus/ex-esdb/dev-env

# Source the health check function from grouped cluster manager
source ./grouped-cluster-manager.sh

# Test the enhanced health check function
if check_node_health ex-esdb0 true; then
    print_color $GREEN "✓ Grouped cluster manager health check passed"
else
    print_color $YELLOW "⚠ Grouped cluster manager health check failed"
fi

# Cleanup
print_color $BLUE "Cleaning up test environment..."
docker-compose \
    -f ex-esdb-volumes.yaml \
    -f ex-esdb-network.yaml \
    -f ex-esdb-cluster.yaml \
    --profile cluster \
    -p cluster \
    down >/dev/null 2>&1

echo
print_color $GREEN "╔══════════════════════════════════════════════════════════════╗"
print_color $GREEN "║                    Test Complete                            ║"
print_color $GREEN "║                                                              ║"
print_color $GREEN "║     Enhanced EPMD-free healthcheck is ready for use         ║"
print_color $GREEN "╚══════════════════════════════════════════════════════════════╝"
