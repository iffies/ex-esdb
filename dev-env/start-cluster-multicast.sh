#!/bin/bash

# ExESDB Cluster Startup Script with Gossip MultiCast
# This script configures and starts both ExESDB cluster and ExESDGater with multicast gossip

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 Starting ExESDB Cluster with Gossip MultiCast...${NC}"

# Source cluster environment variables
if [ -f ".env.cluster" ]; then
    echo -e "${GREEN}📋 Loading cluster configuration...${NC}"
    source .env.cluster
else
    echo -e "${YELLOW}⚠️  .env.cluster file not found, using defaults...${NC}"
    export REG_GH_CLUSTER_SECRET="dev_cluster_secret_2025"
    export REG_GH_DEV_CLIQUE="reg_gh_dev_clique"
fi

# Ensure Docker network exists
echo -e "${BLUE}🌐 Setting up Docker network...${NC}"
docker network create ex-esdb-net --driver bridge --subnet=172.20.0.0/16 2>/dev/null || true

# Build images if they don't exist
echo -e "${BLUE}🔨 Building Docker images...${NC}"
docker-compose -f ex-esdb-cluster.yaml build

# Start ExESDB cluster
echo -e "${GREEN}🎯 Starting ExESDB cluster nodes...${NC}"
docker-compose -f ex-esdb-cluster.yaml --profile cluster up -d

# Wait for cluster to stabilize
echo -e "${YELLOW}⏳ Waiting for cluster to stabilize (30 seconds)...${NC}"
sleep 30

# Check cluster status
echo -e "${BLUE}📊 Checking cluster status...${NC}"
docker-compose -f ex-esdb-cluster.yaml ps

# Optional: Start ExESDGater
echo -e "${BLUE}🚪 Do you want to start ExESDGater? (y/n)${NC}"
read -r start_gater

if [ "$start_gater" = "y" ] || [ "$start_gater" = "yes" ]; then
    echo -e "${GREEN}🚪 Starting ExESDGater...${NC}"
    cd ../ex-esdb-api/dev-env
    docker-compose -f ex-esdb-gater.yaml build
    docker-compose -f ex-esdb-gater.yaml --profile gater up -d
    echo -e "${GREEN}✅ ExESDGater started successfully!${NC}"
    cd ../../ex-esdb/dev-env
fi

echo -e "${GREEN}✅ Cluster setup complete!${NC}"
echo -e "${BLUE}📋 Cluster Configuration:${NC}"
echo -e "  • Strategy: Gossip Broadcast"
echo -e "  • Broadcast Address: 255.255.255.255"
echo -e "  • Port: 45892"
echo -e "  • Network: ex-esdb-net"
echo -e ""
echo -e "${YELLOW}📝 Useful Commands:${NC}"
echo -e "  • View logs: docker-compose -f ex-esdb-cluster.yaml logs -f"
echo -e "  • Stop cluster: docker-compose -f ex-esdb-cluster.yaml down"
echo -e "  • Monitor nodes: docker-compose -f ex-esdb-cluster.yaml ps"
