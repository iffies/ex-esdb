#!/bin/bash

# ExESDB EZ Cluster Manager
# Simple, user-friendly interface for managing ExESDB clusters

set -e

# Colors and formatting
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Cluster configurations
declare -A CLUSTER_CONFIGS=(
    ["core"]="3 nodes (ex-esdb0-2) - Base cluster"
    ["extended"]="2 nodes (ex-esdb10-11) - Extended tier"
    ["massive"]="8 nodes (ex-esdb20-27) - Massive tier"
)

declare -A CLUSTER_NODES=(
    ["core"]="ex-esdb0 ex-esdb1 ex-esdb2"
    ["extended"]="ex-esdb10 ex-esdb11"
    ["massive"]="ex-esdb20 ex-esdb21 ex-esdb22 ex-esdb23 ex-esdb24 ex-esdb25 ex-esdb26 ex-esdb27"
)

# Function to print styled text
print_styled() {
    local style=$1
    local text=$2
    echo -e "${style}${text}${NC}"
}

# Function to print section headers
print_header() {
    echo
    print_styled "$CYAN$BOLD" "╔═══════════════════════════════════════════════════════════════════════════════╗"
    print_styled "$CYAN$BOLD" "║                            ExESDB EZ Cluster Manager                          ║"
    print_styled "$CYAN$BOLD" "║                                                                               ║"
    print_styled "$CYAN$BOLD" "║                        Simple • Fast • Reliable                               ║"
    print_styled "$CYAN$BOLD" "╚═══════════════════════════════════════════════════════════════════════════════╝"
    echo
}

# Function to get container status with emoji
get_container_status() {
    local container_name=$1
    local status=$(docker ps -a --filter "name=$container_name" --format "{{.Status}}" 2>/dev/null)
    
    if [[ -z "$status" ]]; then
        echo -e "${DIM}⚫ Not created${NC}"
        return
    elif [[ "$status" != *"Up"* ]]; then
        echo -e "${RED}🔴 Stopped${NC}"
        return
    fi
    
    # Container is running, get health status
    local health_status=""
    
    # Determine health status
    if [[ "$status" == *"healthy"* ]]; then
        health_status="${GREEN}🟢"
    elif [[ "$status" == *"unhealthy"* ]]; then
        health_status="${YELLOW}🟡"
    elif [[ "$status" == *"starting"* ]]; then
        health_status="${BLUE}🔵"
    else
        health_status="${CYAN}🔵"
    fi
    
    # Display health status only
    if [[ "$status" == *"healthy"* ]]; then
        echo -e "${health_status} Healthy${NC}"
    elif [[ "$status" == *"unhealthy"* ]]; then
        echo -e "${health_status} Unhealthy${NC}"
    elif [[ "$status" == *"starting"* ]]; then
        echo -e "${health_status} Starting${NC}"
    else
        echo -e "${health_status} Running${NC}"
    fi
}

# Function to show cluster status
show_cluster_status() {
    local cluster_type=${1:-"all"}
    
    print_styled "$WHITE$BOLD" "🏗️  Cluster Status Overview"
    echo
    
    # Show all tiers
    for tier in "core" "extended" "massive"; do
        local nodes=(${CLUSTER_NODES[$tier]})
        local -i running=0
        local -i healthy=0
        local total=${#nodes[@]}
        
        print_styled "$PURPLE$BOLD" "📊 ${tier^} Tier (${CLUSTER_CONFIGS[$tier]})"
        
        for node in "${nodes[@]}"; do
            local status=$(get_container_status "$node")
            printf "   %-12s %s\n" "$node" "$status"
            
            if docker ps --filter "name=$node" --filter "status=running" | grep -q "$node"; then
                running=$((running + 1))
                if [[ "$status" == *"Healthy"* ]]; then
                    healthy=$((healthy + 1))
                fi
            fi
        done
        
        # Show tier summary
        if [[ $running -eq 0 ]]; then
            print_styled "$DIM" "   ⚫ Tier not running"
        elif [[ $healthy -eq $total && $running -eq $total ]]; then
            print_styled "$GREEN" "   ✅ Tier fully operational ($healthy/$total healthy)"
        else
            print_styled "$YELLOW" "   ⚠️  Tier partially operational ($running/$total running, $healthy healthy)"
        fi
        echo
    done
    
    # Gateway status
    print_styled "$PURPLE$BOLD" "🌐 Gateway"
    printf "   %-12s %s\n" "ex-esdb-gater" "$(get_container_status ex-esdb-gater)"
    echo
}

# Function to start a cluster tier
start_cluster() {
    local cluster_type=$1
    
    if [[ -z "${CLUSTER_NODES[$cluster_type]}" ]]; then
        print_styled "$RED" "❌ Invalid cluster type: $cluster_type"
        return 1
    fi
    
    print_styled "$YELLOW$BOLD" "🚀 Starting ${cluster_type^} Tier..."
    print_styled "$DIM" "   Configuration: ${CLUSTER_CONFIGS[$cluster_type]}"
    echo
    
    cd "$SCRIPT_DIR"
    
    # Use existing start scripts
    case $cluster_type in
        "core")
            ./start-core-only.sh
            ;;
        "extended")
            ./start-extended-only.sh
            ;;
        "massive")
            ./start-massive-only.sh
            ;;
        *)
            print_styled "$RED" "❌ Unknown cluster type: $cluster_type"
            return 1
            ;;
    esac
    
    print_styled "$GREEN" "🎉 ${cluster_type^} tier startup complete!"
    echo
    
    # Show final status
    show_cluster_status
}

# Function to stop specific cluster tier
stop_specific_cluster() {
    local cluster_type=$1
    
    if [[ -z "${CLUSTER_NODES[$cluster_type]}" ]]; then
        print_styled "$RED" "❌ Invalid cluster type: $cluster_type"
        return 1
    fi
    
    print_styled "$YELLOW$BOLD" "🛑 Stopping ${cluster_type^} Tier..."
    echo
    
    cd "$SCRIPT_DIR"
    
    # Use existing stop scripts
    case $cluster_type in
        "core")
            ./stop-core.sh
            ;;
        "extended")
            ./stop-extended.sh
            ;;
        "massive")
            ./stop-massive.sh
            ;;
        *)
            print_styled "$RED" "❌ Unknown cluster type: $cluster_type"
            return 1
            ;;
    esac
    
    print_styled "$GREEN" "✅ ${cluster_type^} tier stopped successfully!"
    echo
    
    # Show final status
    show_cluster_status
}

# Function to stop all clusters
stop_all_clusters() {
    print_styled "$YELLOW$BOLD" "🛑 Stopping all tiers..."
    
    cd "$SCRIPT_DIR"
    
    # Use existing stop script
    if [[ -f "stop-all.sh" ]]; then
        ./stop-all.sh
    else
        # Stop each tier individually
        print_styled "$BLUE" "⏳ Stopping massive tier..."
        ./stop-massive.sh 2>/dev/null || true
        
        print_styled "$BLUE" "⏳ Stopping extended tier..."
        ./stop-extended.sh 2>/dev/null || true
        
        print_styled "$BLUE" "⏳ Stopping core tier..."
        ./stop-core.sh 2>/dev/null || true
    fi
    
    print_styled "$GREEN" "✅ All tiers stopped successfully!"
}

# Function to restart cluster
restart_cluster() {
    local cluster_type=$1
    
    print_styled "$YELLOW$BOLD" "🔄 Restarting ${cluster_type^} Cluster..."
    
    # Stop first
    print_styled "$BLUE" "⏳ Stopping current clusters..."
    stop_all_clusters > /dev/null 2>&1
    
    # Wait a moment
    print_styled "$BLUE" "⏳ Waiting for clean shutdown..."
    sleep 3
    
    # Start again
    start_cluster "$cluster_type"
}

# Function to scale cluster
scale_cluster() {
    local from_type=$1
    local to_type=$2
    
    if [[ -z "${CLUSTER_NODES[$from_type]}" || -z "${CLUSTER_NODES[$to_type]}" ]]; then
        print_styled "$RED" "❌ Invalid cluster types"
        return 1
    fi
    
    local from_nodes=(${CLUSTER_NODES[$from_type]})
    local to_nodes=(${CLUSTER_NODES[$to_type]})
    
    # Find new nodes to add
    local new_nodes=()
    for node in "${to_nodes[@]}"; do
        if [[ ! " ${from_nodes[*]} " =~ " $node " ]]; then
            new_nodes+=("$node")
        fi
    done
    
    if [[ ${#new_nodes[@]} -eq 0 ]]; then
        print_styled "$YELLOW" "⚠️  No new nodes to add when scaling from $from_type to $to_type"
        return 0
    fi
    
    print_styled "$YELLOW$BOLD" "📈 Scaling from ${from_type^} to ${to_type^} cluster..."
    print_styled "$CYAN" "   Adding nodes: ${new_nodes[*]}"
    print_styled "$DIM" "   Target configuration: ${CLUSTER_CONFIGS[$to_type]}"
    echo
    
    cd "$SCRIPT_DIR"
    
    # Add the new nodes
    print_styled "$BLUE" "⏳ Adding new nodes to cluster..."
    if docker-compose \
        -f ex-esdb-volumes.yaml \
        -f ex-esdb-network.yaml \
        -f ex-esdb-cluster.yaml \
        --profile cluster \
        -p cluster \
        up -d "${new_nodes[@]}" 2>/dev/null; then
        
        print_styled "$GREEN" "✅ Cluster scaled successfully!"
        
        # Wait for new nodes to join
        print_styled "$BLUE" "⏳ Waiting for new nodes to join cluster..."
        for ((i=1; i<=20; i++)); do
            printf "\r${BLUE}   Cluster formation: %d/20s${NC}" "$i"
            sleep 1
        done
        echo
        echo
        
        show_cluster_status "$to_type"
    else
        print_styled "$RED" "❌ Failed to scale cluster"
        return 1
    fi
}

# Function to show health details
show_health() {
    local node=${1:-"all"}
    
    if [[ "$node" == "all" ]]; then
        print_styled "$WHITE$BOLD" "🏥 Health Check Results"
        echo
        
        # Check all nodes across all tiers
        for tier in "core" "extended" "massive"; do
            local nodes=(${CLUSTER_NODES[$tier]})
            for node_name in "${nodes[@]}"; do
            if docker ps --filter "name=$node_name" --filter "status=running" | grep -q "$node_name"; then
                print_styled "$CYAN$BOLD" "📋 $node_name:"
                docker exec "$node_name" /system/check-ex-esdb.sh 2>/dev/null || print_styled "$RED" "   ❌ Health check failed"
                echo
            fi
            done
        done
    else
        if docker ps --filter "name=$node" --filter "status=running" | grep -q "$node"; then
            print_styled "$CYAN$BOLD" "📋 Health check for $node:"
            docker exec "$node" /system/check-ex-esdb.sh
        else
            print_styled "$RED" "❌ Node $node is not running"
        fi
    fi
}

# Function to show logs
show_logs() {
    local node=${1:-"all"}
    local follow=${2:-false}
    
    if [[ "$node" == "all" ]]; then
        print_styled "$YELLOW$BOLD" "📜 Showing logs for all nodes..."
        if [[ "$follow" == "true" ]]; then
            docker-compose -p cluster logs -f
        else
            docker-compose -p cluster logs --tail=30
        fi
    else
        if docker ps --filter "name=$node" | grep -q "$node"; then
            print_styled "$YELLOW$BOLD" "📜 Showing logs for $node..."
            if [[ "$follow" == "true" ]]; then
                docker logs "$node" -f
            else
                docker logs "$node" --tail=30
            fi
        else
            print_styled "$RED" "❌ Node $node not found"
        fi
    fi
}

# Function to show quick status
show_quick_status() {
    local -i total_running=0
    local -i total_healthy=0
    
    # Count all ex-esdb containers across all tiers
    for tier in "core" "extended" "massive"; do
        local nodes=(${CLUSTER_NODES[$tier]})
        for node in "${nodes[@]}"; do
        if docker ps --filter "name=$node" --filter "status=running" | grep -q "$node"; then
            total_running=$((total_running + 1))
            local status=$(get_container_status "$node")
            if [[ "$status" == *"Healthy"* ]]; then
                total_healthy=$((total_healthy + 1))
            fi
        fi
        done
    done
    
    if [[ $total_running -eq 0 ]]; then
        print_styled "$DIM" "⚫ No clusters running"
    elif [[ $total_healthy -eq $total_running ]]; then
        print_styled "$GREEN" "✅ $total_running nodes running, all healthy"
    else
        print_styled "$YELLOW" "⚠️  $total_running nodes running, $total_healthy healthy"
    fi
}

# Function to show main menu
show_menu() {
    clear
    print_header
    show_cluster_status
    
    print_styled "$WHITE$BOLD" "🎛️  Quick Actions:"
    echo
    
    print_styled "$GREEN$BOLD" "   Start Tiers:"
    print_styled "$GREEN" "     [1] 🔹 Core Tier        (3 nodes: ex-esdb0-2)"
    print_styled "$GREEN" "     [2] 🔸 Extended Tier    (2 nodes: ex-esdb10-11)"
    print_styled "$GREEN" "     [3] 🔶 Massive Tier     (8 nodes: ex-esdb20-27)"
    echo
    
    print_styled "$RED$BOLD" "   Stop Tiers:"
    print_styled "$RED" "     [4] 🛑 Stop Core Tier"
    print_styled "$RED" "     [5] 🛑 Stop Extended Tier"
    print_styled "$RED" "     [6] 🛑 Stop Massive Tier"
    print_styled "$RED" "     [7] 🛑 Stop All Tiers"
    echo
    
    print_styled "$YELLOW$BOLD" "   Restart Tiers:"
    print_styled "$YELLOW" "     [8] 🔄 Restart Core"
    print_styled "$YELLOW" "     [9] 🔄 Restart Extended"
    print_styled "$YELLOW" "     [10] 🔄 Restart Massive"
    echo
    
    print_styled "$PURPLE$BOLD" "   Monitoring:"
    print_styled "$PURPLE" "     [h] 🏥 Health Check"
    print_styled "$PURPLE" "     [l] 📜 Recent Logs"
    print_styled "$PURPLE" "     [f] 👁️  Follow Logs"
    print_styled "$PURPLE" "     [w] 📡 Live Monitor (auto-refresh)"
    print_styled "$PURPLE" "     [s] 🔄 Refresh Status"
    echo
    
    print_styled "$WHITE$BOLD" "     [q] 👋 Quit"
    echo
    
    print_styled "$DIM" "💡 Tip: Use 'logs <node>' or 'health <node>' for specific nodes"
    echo
}

# Function to handle user input
handle_input() {
    local choice=$1
    
    case $choice in
        1) start_cluster "core" ;;
        2) start_cluster "extended" ;;
        3) start_cluster "massive" ;;
        4) stop_specific_cluster "core" ;;
        5) stop_specific_cluster "extended" ;;
        6) stop_specific_cluster "massive" ;;
        7) stop_all_clusters ;;
        8) restart_cluster "core" ;;
        9) restart_cluster "extended" ;;
        10) restart_cluster "massive" ;;
        h|H) show_health ;;
        l|L) show_logs ;;
        f|F) show_logs "all" "true" ;;
        s|S) return 0 ;;  # Just refresh
        logs*)
            local node=$(echo "$choice" | cut -d' ' -f2)
            if [[ -n "$node" ]]; then
                show_logs "$node"
            else
                show_logs
            fi
            ;;
        health*)
            local node=$(echo "$choice" | cut -d' ' -f2)
            if [[ -n "$node" ]]; then
                show_health "$node"
            else
                show_health
            fi
            ;;
        q|Q) 
            print_styled "$GREEN$BOLD" "👋 Thanks for using EZ Cluster Manager!"
            exit 0
            ;;
        *)
            print_styled "$RED" "❌ Invalid choice: '$choice'"
            print_styled "$YELLOW" "💡 Try: 1-10, h, l, f, s, q, or 'logs <node>'"
            ;;
    esac
}

# Function to pause for user input
pause_for_input() {
    echo
    print_styled "$DIM" "Press Enter to continue..."
    read -r
}

# Main interactive loop
main_interactive() {
    while true; do
        show_menu
        echo -n "$(print_styled "$WHITE$BOLD" "🎯 Your choice: ")"
        read -r choice
        echo
        
        if [[ "$choice" == "q" || "$choice" == "Q" ]]; then
            print_styled "$GREEN$BOLD" "👋 Thanks for using EZ Cluster Manager!"
            exit 0
        elif [[ "$choice" == "s" || "$choice" == "S" ]]; then
            continue  # Just refresh
        elif [[ "$choice" == "f" || "$choice" == "F" ]]; then
            show_logs "all" "true"
        else
            handle_input "$choice"
            if [[ "$choice" != "f" && "$choice" != "F" ]]; then
                pause_for_input
            fi
        fi
    done
}

# Command-line interface
main_cli() {
    local command=$1
    shift
    
    case $command in
        start)
            local cluster_type=${1:-"core"}
            start_cluster "$cluster_type"
            ;;
        stop)
            local cluster_type=$1
            if [[ -n "$cluster_type" && "$cluster_type" != "all" ]]; then
                stop_specific_cluster "$cluster_type"
            else
                stop_all_clusters
            fi
            ;;
        restart)
            local cluster_type=${1:-"core"}
            restart_cluster "$cluster_type"
            ;;
        scale)
            local from_type=$1
            local to_type=$2
            if [[ -n "$from_type" && -n "$to_type" ]]; then
                scale_cluster "$from_type" "$to_type"
            else
                print_styled "$RED" "❌ Usage: $0 scale <from-type> <to-type>"
                print_styled "$YELLOW" "   Example: $0 scale core extended"
                exit 1
            fi
            ;;
        status)
            show_cluster_status
            ;;
        health)
            local node=${1:-"all"}
            show_health "$node"
            ;;
        logs)
            local node=${1:-"all"}
            local follow=${2:-false}
            show_logs "$node" "$follow"
            ;;
        *)
            print_styled "$CYAN$BOLD" "ExESDB EZ Cluster Manager"
            echo
            print_styled "$WHITE" "Usage: $0 [command] [options]"
            echo
            print_styled "$WHITE" "Commands:"
            print_styled "$GREEN" "  start [core|extended|massive]  - Start a cluster"
            print_styled "$BLUE" "  scale <from> <to>              - Scale cluster between sizes"
            print_styled "$YELLOW" "  stop                           - Stop all clusters"
            print_styled "$YELLOW" "  restart [core|extended|massive] - Restart cluster"
            print_styled "$PURPLE" "  status                         - Show cluster status"
            print_styled "$PURPLE" "  health [node]                  - Show health checks"
            print_styled "$PURPLE" "  logs [node] [follow]           - Show logs"
            echo
            print_styled "$CYAN" "Examples:"
            print_styled "$DIM" "  $0 start core                  # Start 3-node cluster"
            print_styled "$DIM" "  $0 scale core extended         # Scale from 3 to 5 nodes"
            print_styled "$DIM" "  $0 health ex-esdb0             # Check specific node"
            print_styled "$DIM" "  $0 logs ex-esdb1 follow        # Follow logs for node"
            echo
            print_styled "$CYAN" "Run without arguments for interactive mode"
            ;;
    esac
}

# Main entry point
if [[ $# -eq 0 ]]; then
    # Interactive mode
    main_interactive
else
    # CLI mode
    main_cli "$@"
fi
