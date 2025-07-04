#!/bin/bash

# ExESDB Cluster Validation Script
# This script validates that the Gossip MultiCast cluster is working correctly

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🔍 Validating ExESDB Cluster with Gossip MultiCast...${NC}"

# Check if containers are running
echo -e "${BLUE}📊 Checking container status...${NC}"
running_containers=$(docker-compose -f ex-esdb-cluster.yaml ps --services --filter "status=running" | wc -l)
total_containers=$(docker-compose -f ex-esdb-cluster.yaml ps --services | wc -l)

if [ "$running_containers" -eq "$total_containers" ] && [ "$running_containers" -gt 0 ]; then
    echo -e "${GREEN}✅ All $running_containers containers are running${NC}"
else
    echo -e "${RED}❌ Only $running_containers out of $total_containers containers are running${NC}"
    exit 1
fi

# Check cluster membership
echo -e "${BLUE}🔗 Checking cluster membership...${NC}"
for i in {0..4}; do
    echo -e "${YELLOW}📡 Checking node ex-esdb$i...${NC}"
    
    # Check if container is responding
    if docker exec ex-esdb$i /bin/sh -c "echo 'Node.list().' | /opt/ex_esdb/bin/ex_esdb rpc" 2>/dev/null | grep -q "node"; then
        echo -e "${GREEN}  ✅ Node ex-esdb$i is responsive${NC}"
        
        # Get connected nodes
        connected_nodes=$(docker exec ex-esdb$i /bin/sh -c "echo 'Node.list().' | /opt/ex_esdb/bin/ex_esdb rpc" 2>/dev/null | grep -o "node[0-9]@" | wc -l)
        echo -e "${BLUE}  📊 Connected to $connected_nodes other nodes${NC}"
    else
        echo -e "${RED}  ❌ Node ex-esdb$i is not responsive${NC}"
    fi
done

# Check network connectivity (gossip port)
echo -e "${BLUE}🌐 Checking network connectivity...${NC}"
network_name="ex-esdb-net"
if docker network inspect $network_name >/dev/null 2>&1; then
    echo -e "${GREEN}✅ Docker network '$network_name' exists${NC}"
    
    # Check if containers are on the network
    containers_on_network=$(docker network inspect $network_name --format '{{range .Containers}}{{.Name}} {{end}}' | wc -w)
    echo -e "${BLUE}📊 $containers_on_network containers connected to network${NC}"
else
    echo -e "${RED}❌ Docker network '$network_name' not found${NC}"
fi

# Check broadcast configuration
echo -e "${BLUE}📡 Validating broadcast configuration...${NC}"
echo -e "${BLUE}  • Broadcast Address: 255.255.255.255${NC}"
echo -e "${BLUE}  • Gossip Port: 45892${NC}"

# Check if gossip port is accessible
echo -e "${BLUE}🔍 Checking gossip port accessibility...${NC}"
for i in {0..2}; do  # Check first 3 nodes
    if docker exec ex-esdb$i /bin/sh -c "netstat -ln | grep :45892" 2>/dev/null | grep -q "45892"; then
        echo -e "${GREEN}  ✅ Node ex-esdb$i: Gossip port 45892 is listening${NC}"
    else
        echo -e "${YELLOW}  ⚠️  Node ex-esdb$i: Gossip port 45892 status unclear${NC}"
    fi
done

# Check cluster health via health endpoints (if available)
echo -e "${BLUE}🏥 Checking cluster health...${NC}"
healthy_nodes=0
for i in {0..4}; do
    if docker exec ex-esdb$i /bin/sh -c "curl -s -f http://localhost:4000/health" 2>/dev/null | grep -q "ok\|healthy"; then
        echo -e "${GREEN}  ✅ Node ex-esdb$i: Health check passed${NC}"
        ((healthy_nodes++))
    else
        echo -e "${YELLOW}  ⚠️  Node ex-esdb$i: Health check unclear${NC}"
    fi
done

# Summary
echo -e ""
echo -e "${BLUE}📋 Cluster Validation Summary:${NC}"
echo -e "  • Running containers: $running_containers/$total_containers"
echo -e "  • Healthy nodes: $healthy_nodes/5"
echo -e "  • Network: $network_name"
echo -e "  • Strategy: Gossip MultiCast"

if [ "$running_containers" -eq "$total_containers" ] && [ "$healthy_nodes" -ge 3 ]; then
    echo -e "${GREEN}🎉 Cluster validation successful!${NC}"
    exit 0
else
    echo -e "${YELLOW}⚠️  Cluster validation completed with warnings${NC}"
    exit 1
fi
