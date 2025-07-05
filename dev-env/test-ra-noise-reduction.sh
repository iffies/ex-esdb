#!/bin/bash

# Test Ra Noise Reduction Script
# This script tests the effectiveness of Ra noise filtering

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
print_color $CYAN "║                Ra Noise Reduction Test                      ║"
print_color $CYAN "║                                                              ║"
print_color $CYAN "║         Testing Ra/Khepri log filtering effectiveness       ║"
print_color $CYAN "╚══════════════════════════════════════════════════════════════╝"
echo

# Test 1: Build image with new logging configuration
print_color $BLUE "Test 1: Building image with Ra noise reduction..."
cd /home/rl/work/github.com/beam-campus/ex-esdb/system
if docker build -t local/ex-esdb . >/dev/null 2>&1; then
    print_color $GREEN "✓ Image built successfully with noise reduction"
else
    print_color $RED "✗ Failed to build image"
    exit 1
fi

# Test 2: Start cluster and monitor logs for noise
print_color $BLUE "Test 2: Starting single node and monitoring Ra noise..."
cd /home/rl/work/github.com/beam-campus/ex-esdb/dev-env

# Clean up any existing containers
docker-compose \
    -f ex-esdb-volumes.yaml \
    -f ex-esdb-network.yaml \
    -f ex-esdb-cluster.yaml \
    --profile cluster \
    -p cluster \
    down >/dev/null 2>&1

# Start single node
docker-compose \
    -f ex-esdb-volumes.yaml \
    -f ex-esdb-network.yaml \
    -f ex-esdb-cluster.yaml \
    --profile cluster \
    -p cluster \
    up -d ex-esdb0 >/dev/null 2>&1

print_color $YELLOW "Waiting for node to start and generate logs..."
sleep 15

# Test 3: Analyze logs for Ra noise
print_color $BLUE "Test 3: Analyzing logs for Ra/Khepri noise..."

# Get logs from the last 15 seconds
logs=$(docker logs ex-esdb0 --since 15s 2>&1)

# Count Ra-related messages
ra_count=$(echo "$logs" | grep -i "ra_" | wc -l)
heartbeat_count=$(echo "$logs" | grep -i "heartbeat" | wc -l)
khepri_count=$(echo "$logs" | grep -i "khepri" | wc -l)
append_entries_count=$(echo "$logs" | grep -i "append_entries" | wc -l)

print_color $CYAN "Log Analysis Results:"
printf "  Ra messages: %d\n" $ra_count
printf "  Heartbeat messages: %d\n" $heartbeat_count
printf "  Khepri messages: %d\n" $khepri_count
printf "  Append entries messages: %d\n" $append_entries_count

total_noise=$((ra_count + heartbeat_count + append_entries_count))

if [[ $total_noise -lt 10 ]]; then
    print_color $GREEN "✓ Ra noise successfully reduced (${total_noise} noisy messages)"
elif [[ $total_noise -lt 50 ]]; then
    print_color $YELLOW "⚠ Ra noise partially reduced (${total_noise} noisy messages)"
else
    print_color $RED "✗ Ra noise still high (${total_noise} noisy messages)"
fi

# Test 4: Show sample of actual log output
print_color $BLUE "Test 4: Sample of filtered log output (last 10 lines):"
print_color $WHITE "---"
docker logs ex-esdb0 --tail 10 2>&1 | head -10
print_color $WHITE "---"

# Test 5: Check if important messages still come through
print_color $BLUE "Test 5: Checking if important messages still appear..."

# Look for startup and cluster formation messages
startup_count=$(echo "$logs" | grep -i "UP\|started\|ready" | wc -l)
error_count=$(echo "$logs" | grep -i "error\|failed" | wc -l)

printf "  Startup messages: %d\n" $startup_count
printf "  Error messages: %d\n" $error_count

if [[ $startup_count -gt 0 ]]; then
    print_color $GREEN "✓ Important startup messages still visible"
else
    print_color $YELLOW "⚠ No startup messages found (may be too early)"
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
if [[ $total_noise -lt 20 ]]; then
    print_color $GREEN "╔══════════════════════════════════════════════════════════════╗"
    print_color $GREEN "║                  Ra Noise Reduction Success                 ║"
    print_color $GREEN "║                                                              ║"
    print_color $GREEN "║            Ra consensus logging noise significantly          ║"
    print_color $GREEN "║                          reduced!                           ║"
    print_color $GREEN "╚══════════════════════════════════════════════════════════════╝"
else
    print_color $YELLOW "╔══════════════════════════════════════════════════════════════╗"
    print_color $YELLOW "║               Ra Noise Reduction Partial                    ║"
    print_color $YELLOW "║                                                              ║"
    print_color $YELLOW "║        Some noise reduction achieved, but more tuning       ║"
    print_color $YELLOW "║                        may be needed                        ║"
    print_color $YELLOW "╚══════════════════════════════════════════════════════════════╝"
fi
