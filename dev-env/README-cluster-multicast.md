# ExESDB Cluster with Gossip MultiCast

This document explains how to set up and run ExESDB and ExESDGater with Gossip MultiCast clustering via libcluster.

## Overview

The setup uses **Cluster.Strategy.Gossip** with multicast discovery to automatically form a cluster between:
- ExESDB cluster nodes (ex-esdb0 through ex-esdb4)
- ExESDGater (when connected to the same network)

## Configuration

### Gossip Broadcast Settings

- **Strategy**: `Cluster.Strategy.Gossip`
- **Broadcast Address**: `255.255.255.255`
- **Port**: `45892`
- **Broadcast Only**: `true`
- **Network**: `ex-esdb-net` (Docker bridge network)
- **Security**: Shared secret via `EX_ESDB_CLUSTER_SECRET`

### Environment Variables

Create `.env.cluster` file or set these environment variables:

```bash
# Cluster security
export EX_ESDB_CLUSTER_SECRET="dev_cluster_secret_2025"
export EX_ESDB_COOKIE="reg_greenhouse_clique"
export RELEASE_COOKIE="reg_greenhouse_clique"

# Gossip configuration  
export EX_ESDB_GOSSIP_MULTICAST_ADDR="255.255.255.255"
export GOSSIP_PORT="45892"
```

## Quick Start

### 1. Automated Startup

Use the provided startup script:

```bash
./start-cluster-multicast.sh
```

This script will:
- Load environment variables
- Create Docker network
- Build images
- Start ExESDB cluster
- Optionally start ExESDGater

### 2. Manual Startup

#### Start ExESDB Cluster

```bash
# Source environment variables
source .env.cluster

# Create network
docker network create ex-esdb-net --driver bridge --subnet=172.20.0.0/16

# Start cluster
docker-compose -f ex-esdb-cluster.yaml --profile cluster up -d
```

#### Start ExESDGater

```bash
cd ../ex-esdb-api/dev-env
docker-compose -f ex-esdb-gater.yaml --profile gater up -d
```

## Validation

### Validate Cluster

Run the validation script:

```bash
./validate-cluster.sh
```

This checks:
- Container status
- Network connectivity
- Cluster membership
- Health endpoints
- Gossip port accessibility

### Manual Validation

#### Check Running Containers

```bash
docker-compose -f ex-esdb-cluster.yaml ps
```

#### Check Cluster Membership

```bash
# Check connected nodes from any ExESDB container
docker exec ex-esdb0 /bin/sh -c "echo 'Node.list().' | /opt/ex_esdb/bin/ex_esdb rpc"
```

#### Check Logs

```bash
# View all cluster logs
docker-compose -f ex-esdb-cluster.yaml logs -f

# View specific node logs
docker-compose -f ex-esdb-cluster.yaml logs -f ex-esdb0
```

## Architecture

### Cluster Topology

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   ExESDB0   │    │   ExESDB1   │    │   ExESDB2   │
│   (node0)   │◄──►│   (node1)   │◄──►│   (node2)   │
└─────────────┘    └─────────────┘    └─────────────┘
       ▲                  ▲                  ▲
       │                  │                  │
       ▼                  ▼                  ▼
    Gossip MultiCast Network (255.255.255.255:45892)
       ▲                  ▲                  ▲
       │                  │                  │
       ▼                  ▼                  ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   ExESDB3   │    │   ExESDB4   │    │ ExESDGater  │
│   (node3)   │◄──►│   (node4)   │◄──►│  (gater)    │
└─────────────┘    └─────────────┘    └─────────────┘
```

### Network Configuration

- **Docker Network**: `ex-esdb-net` (bridge)
- **Subnet**: `172.20.0.0/16`
- **Gossip Communication**: UDP multicast on port 45892
- **Application Communication**: TCP on application ports

## Configuration Files

### ExESDB Configuration

**dev.exs** and **prod.exs**:
```elixir
config :libcluster,
  topologies: [
    ex_esdb_cluster: [
      strategy: Cluster.Strategy.Gossip,
      config: [
        port: 45_892,
        if_addr: "0.0.0.0",
        multicast_addr: System.get_env("EX_ESDB_GOSSIP_MULTICAST_ADDR") || "255.255.255.255",
        multicast_ttl: 1,
        secret: System.get_env("EX_ESDB_CLUSTER_SECRET") || "dev_cluster_secret"
      ]
    ]
  ]
```

### ExESDGater Configuration

Same configuration as ExESDB to ensure compatibility.

## Troubleshooting

### Common Issues

#### 1. Nodes Not Discovering Each Other

**Symptoms**: Nodes remain isolated, `Node.list()` returns empty list

**Solutions**:
- Check Docker network connectivity
- Verify multicast address configuration
- Ensure gossip port (45892) is not blocked
- Check cluster secret matches across all nodes

#### 2. Permission Denied for Multicast

**Symptoms**: Gossip fails to bind to multicast address

**Solutions**:
- Run containers with appropriate network permissions
- Check Docker daemon configuration
- Verify multicast support on host system

#### 3. Split Brain Scenarios

**Symptoms**: Multiple cluster coordinators

**Solutions**:
- Verify ClusterCoordinator logic is working
- Check network partitions
- Review libcluster logs for coordination issues

### Diagnostic Commands

```bash
# Check network connectivity between containers
docker exec ex-esdb0 ping ex-esdb1

# Check gossip port
docker exec ex-esdb0 netstat -ln | grep 45892

# Check multicast group membership
docker exec ex-esdb0 ip maddr

# View detailed libcluster logs
docker-compose -f ex-esdb-cluster.yaml logs ex-esdb0 | grep -i "libcluster\|gossip"
```

## Advanced Configuration

### Production Considerations

1. **Security**: Use strong, unique cluster secrets
2. **Network**: Consider firewall rules for multicast traffic
3. **Monitoring**: Set up cluster health monitoring
4. **Backup**: Implement data persistence strategies

### Scaling

To add more nodes:

1. Add service definition to `ex-esdb-cluster.yaml`
2. Ensure consistent environment variables
3. Connect to same Docker network
4. Use same gossip configuration

## References

- [libcluster Documentation](https://hexdocs.pm/libcluster/)
- [Cluster.Strategy.Gossip](https://hexdocs.pm/libcluster/Cluster.Strategy.Gossip.html)
- [ExESDB Documentation](../system/guides/getting_started.md)
