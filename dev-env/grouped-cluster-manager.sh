#!/bin/bash

# ExESDB Grouped Cluster Manager
# Manages cluster startup in optimal groups for Raft consensus

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Function to print colored text
print_color() {
    local color=$1
    local text=$2
    echo -e "${color}${text}${NC}"
}

# Function to print header
print_header() {
    clear
    print_color $CYAN "╔══════════════════════════════════════════════════════════════╗"
    print_color $CYAN "║                ExESDB Grouped Cluster Manager               ║"
    print_color $CYAN "║                                                              ║"
    print_color $CYAN "║              Optimal Raft Consensus Startup                 ║"
    print_color $CYAN "╚══════════════════════════════════════════════════════════════╝"
    echo
}

# Function to get container status
get_container_status() {
    local container_name=$1
    local status=$(docker ps -a --filter "name=$container_name" --format "{{.Status}}" 2>/dev/null)
    
    if [[ -z "$status" ]]; then
        echo -e "${BLUE}●${NC} Not created"
    elif [[ "$status" == *"Up"* && "$status" == *"healthy"* ]]; then
        echo -e "${GREEN}●${NC} Healthy"
    elif [[ "$status" == *"Up"* && "$status" == *"unhealthy"* ]]; then
        echo -e "${YELLOW}●${NC} Unhealthy"
    elif [[ "$status" == *"Up"* ]]; then
        echo -e "${CYAN}●${NC} Starting"
    else
        echo -e "${RED}●${NC} Stopped"
    fi
}

# Function to show cluster status
show_cluster_status() {
    print_color $WHITE "Cluster Status:"
    echo
    
    print_color $CYAN "Core Group (3-node quorum):"
    printf "  %-12s %s\n" "ex-esdb0" "$(get_container_status ex-esdb0)"
    printf "  %-12s %s\n" "ex-esdb1" "$(get_container_status ex-esdb1)"
    printf "  %-12s %s\n" "ex-esdb2" "$(get_container_status ex-esdb2)"
    
    echo
    print_color $CYAN "Expansion Group:"
    printf "  %-12s %s\n" "ex-esdb3" "$(get_container_status ex-esdb3)"
    printf "  %-12s %s\n" "ex-esdb4" "$(get_container_status ex-esdb4)"
    
    echo
    print_color $CYAN "Gateway:"
    printf "  %-12s %s\n" "ex-esdb-gater" "$(get_container_status ex-esdb-gater)"
}

# Function to start core cluster (3 nodes)
start_core_cluster() {
    print_color $YELLOW "Starting Core Cluster (3-node quorum)..."
    echo
    
    cd "$SCRIPT_DIR"
    
    # Start only the core 3-node group
    print_color $CYAN "Starting core nodes: ex-esdb0, ex-esdb1, ex-esdb2"
    docker-compose \
        -f ex-esdb-volumes.yaml \
        -f ex-esdb-network.yaml \
        -f ex-esdb-cluster.yaml \
        --profile cluster \
        -p cluster \
        up -d ex-esdb0 ex-esdb1 ex-esdb2
    
    if [[ $? -eq 0 ]]; then
        print_color $GREEN "✓ Core cluster started successfully!"
        echo
        print_color $CYAN "Waiting for cluster formation..."
        
        # Wait a bit for cluster to form
        for i in {1..30}; do
            echo -n "."
            sleep 1
        done
        echo
        
        print_color $CYAN "Checking cluster status..."
        check_cluster_health
    else
        print_color $RED "✗ Failed to start core cluster"
    fi
}

# Function to scale cluster (add expansion nodes)
scale_cluster() {
    print_color $YELLOW "Scaling cluster (adding expansion nodes)..."
    echo
    
    cd "$SCRIPT_DIR"
    
    # Check if core cluster is running
    local core_running=0
    for node in ex-esdb0 ex-esdb1 ex-esdb2; do
        if docker ps --filter "name=$node" --filter "status=running" | grep -q $node; then
            ((core_running++))
        fi
    done
    
    if [[ $core_running -lt 3 ]]; then
        print_color $RED "Error: Core cluster is not fully running. Start core cluster first."
        return 1
    fi
    
    # Start expansion nodes
    print_color $CYAN "Adding expansion nodes: ex-esdb3, ex-esdb4"
    docker-compose \
        -f ex-esdb-volumes.yaml \
        -f ex-esdb-network.yaml \
        -f ex-esdb-cluster.yaml \
        --profile cluster \
        -p cluster \
        up -d ex-esdb3 ex-esdb4
    
    if [[ $? -eq 0 ]]; then
        print_color $GREEN "✓ Cluster scaled successfully!"
    else
        print_color $RED "✗ Failed to scale cluster"
    fi
}

# Function to start full cluster at once
start_full_cluster() {
    print_color $YELLOW "Starting Full Cluster (5 nodes)..."
    echo
    
    cd "$SCRIPT_DIR"
    
    print_color $CYAN "Starting all nodes simultaneously"
    docker-compose \
        -f ex-esdb-volumes.yaml \
        -f ex-esdb-network.yaml \
        -f ex-esdb-cluster.yaml \
        --profile cluster \
        -p cluster \
        up -d
    
    if [[ $? -eq 0 ]]; then
        print_color $GREEN "✓ Full cluster started successfully!"
    else
        print_color $RED "✗ Failed to start full cluster"
    fi
}

# Function to stop cluster
stop_cluster() {
    print_color $YELLOW "Stopping cluster..."
    echo
    
    cd "$SCRIPT_DIR"
    
    docker-compose \
        -f ex-esdb-volumes.yaml \
        -f ex-esdb-network.yaml \
        -f ex-esdb-cluster.yaml \
        --profile cluster \
        -p cluster \
        down
    
    if [[ $? -eq 0 ]]; then
        print_color $GREEN "✓ Cluster stopped successfully!"
    else
        print_color $RED "✗ Failed to stop cluster"
    fi
}

# Function to check individual node health using enhanced healthcheck
check_node_health() {
    local node=$1
    local quiet=${2:-false}
    
    if ! docker ps --filter "name=$node" --filter "status=running" | grep -q $node; then
        if [[ "$quiet" != "true" ]]; then
            print_color $RED "✗ $node is not running"
        fi
        return 1
    fi
    
    # Use the enhanced healthcheck script
    if docker exec $node /system/check-ex-esdb.sh >/dev/null 2>&1; then
        if [[ "$quiet" != "true" ]]; then
            print_color $GREEN "✓ $node is healthy"
        fi
        return 0
    else
        if [[ "$quiet" != "true" ]]; then
            print_color $RED "✗ $node failed health check"
        fi
        return 1
    fi
}

# Function to check cluster health
check_cluster_health() {
    print_color $CYAN "Checking cluster health..."
    echo
    
    local healthy_nodes=0
    local total_running=0
    local core_healthy=0
    
    # Check core nodes
    print_color $CYAN "Core Group (3-node quorum):"
    for node in ex-esdb0 ex-esdb1 ex-esdb2; do
        if docker ps --filter "name=$node" --filter "status=running" | grep -q $node; then
            ((total_running++))
            if check_node_health $node; then
                ((healthy_nodes++))
                ((core_healthy++))
            fi
        else
            print_color $RED "✗ $node is not running"
        fi
    done
    
    echo
    print_color $CYAN "Expansion Group:"
    for node in ex-esdb3 ex-esdb4; do
        if docker ps --filter "name=$node" --filter "status=running" | grep -q $node; then
            ((total_running++))
            check_node_health $node
            if [[ $? -eq 0 ]]; then
                ((healthy_nodes++))
            fi
        else
            print_color $RED "✗ $node is not running"
        fi
    done
    
    echo
    print_color $WHITE "Summary:"
    printf "  Running nodes: %d\n" $total_running
    printf "  Healthy nodes: %d\n" $healthy_nodes
    printf "  Core healthy: %d/3\n" $core_healthy
    
    if [[ $core_healthy -ge 3 ]]; then
        print_color $GREEN "✓ Core cluster has quorum ($core_healthy/3 nodes)"
    elif [[ $core_healthy -ge 2 ]]; then
        print_color $YELLOW "⚠ Core cluster has partial quorum ($core_healthy/3 nodes)"
    else
        print_color $RED "✗ Core cluster lacks quorum ($core_healthy/3 nodes)"
    fi
    
    if [[ $healthy_nodes -ge 3 ]]; then
        print_color $GREEN "✓ Overall cluster is healthy ($healthy_nodes nodes)"
        return 0
    else
        print_color $RED "✗ Overall cluster is unhealthy ($healthy_nodes nodes)"
        return 1
    fi
}

# Function to show logs
show_logs() {
    local node=$1
    if [[ -n "$node" ]]; then
        print_color $YELLOW "Showing logs for $node..."
        docker logs $node -f
    else
        print_color $YELLOW "Showing logs for all cluster nodes..."
        docker-compose -p cluster logs -f
    fi
}

# Function to show main menu
show_menu() {
    print_header
    show_cluster_status
    echo
    print_color $WHITE "Available Actions:"
    echo
    print_color $GREEN "  [s] Show Status"
    print_color $GREEN "  [h] Check Cluster Health"
    echo
    print_color $BLUE "  [c] Start Core Cluster (3 nodes)"
    print_color $BLUE "  [e] Scale Cluster (add 2 nodes)"
    print_color $BLUE "  [f] Start Full Cluster (5 nodes)"
    echo
    print_color $YELLOW "  [stop] Stop Cluster"
    print_color $YELLOW "  [restart] Restart Full Cluster"
    echo
    print_color $CYAN "  [logs] Show All Logs"
    print_color $CYAN "  [log0] Show ex-esdb0 Logs"
    print_color $CYAN "  [log1] Show ex-esdb1 Logs"
    print_color $CYAN "  [log2] Show ex-esdb2 Logs"
    echo
    print_color $WHITE "  [q] Quit"
    echo
}

# Main loop
main() {
    while true; do
        show_menu
        echo -n "Enter your choice: "
        read -r choice
        echo

        case $choice in
            s|S) continue ;;
            h|H) check_cluster_health ;;
            c|C) start_core_cluster ;;
            e|E) scale_cluster ;;
            f|F) start_full_cluster ;;
            stop|STOP) stop_cluster ;;
            restart|RESTART) 
                stop_cluster
                echo
                start_full_cluster
                ;;
            logs|LOGS) show_logs ;;
            log0) show_logs ex-esdb0 ;;
            log1) show_logs ex-esdb1 ;;
            log2) show_logs ex-esdb2 ;;
            q|Q)
                print_color $GREEN "Goodbye!"
                exit 0
                ;;
            *)
                print_color $RED "Invalid choice. Please try again."
                ;;
        esac

        echo
        print_color $CYAN "Press Enter to continue..."
        read
    done
}

# Check if running as script
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
