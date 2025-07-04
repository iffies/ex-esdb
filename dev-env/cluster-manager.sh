#!/bin/bash

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

# Environment mode (dev/prod)
ENVIRONMENT_MODE="dev"

# Cluster configurations
# Format: "files,project_name,description"
declare -A CLUSTERS=(
    ["cluster1"]="ex-esdb-volumes.yaml,ex-esdb-network.yaml,ex-esdb-cluster.yaml,cluster,nodes 0-4"
    ["cluster2"]="ex-esdb-volumes2.yaml,ex-esdb-cluster2.yaml,cluster2,nodes 10-17"
    ["swarm"]="ex-esdb-swarm.yaml,swarm,swarm nodes"
)

# Production override files
declare -A PROD_OVERRIDES=(
    ["cluster1"]="ex-esdb-prod-override.yaml"
    ["cluster2"]="ex-esdb-prod-override.yaml"
)

# Function to print colored text
print_color() {
    local color=$1
    local text=$2
    echo -e "${color}${text}${NC}"
}

# Helper function to parse cluster configuration
get_cluster_config() {
    local cluster_name=$1
    local config_type=$2  # project, description, files
    
    IFS=',' read -ra config <<< "${CLUSTERS[$cluster_name]}"
    
    case $cluster_name in
        "cluster1")
            # Format: volumes.yaml,network.yaml,cluster.yaml,project,description
            case $config_type in
                "project") echo "${config[3]}" ;;
                "description") echo "${config[4]}" ;;
                "files") echo "${config[0]},${config[1]},${config[2]}" ;;
            esac
            ;;
        "cluster2")
            # Format: volumes2.yaml,cluster2.yaml,project,description
            case $config_type in
                "project") echo "${config[2]}" ;;
                "description") echo "${config[3]}" ;;
                "files") echo "${config[0]},${config[1]}" ;;
            esac
            ;;
        "swarm")
            # Format: swarm.yaml,project,description
            case $config_type in
                "project") echo "${config[1]}" ;;
                "description") echo "${config[2]}" ;;
                "files") echo "${config[0]}" ;;
            esac
            ;;
    esac
}

# Function to print header
print_header() {
    clear
    print_color $CYAN "╔══════════════════════════════════════════════════════════════╗"
    print_color $CYAN "║                    ExESDB Cluster Manager                    ║"
    print_color $CYAN "║                                                              ║"
    print_color $CYAN "║              LibCluster + Khepri Coordination                ║"
    print_color $CYAN "╚══════════════════════════════════════════════════════════════╝"
    echo
}

# Function to get cluster status
get_cluster_status() {
    local cluster_name=$1
    local project_name=$2
    
    # Different filtering strategies for different project naming schemes
    local filter_pattern
    if [[ "$project_name" == "cluster" ]]; then
        # For cluster project, containers are named ex-esdb0, ex-esdb1, etc.
        filter_pattern="name=^ex-esdb[0-4]$"
    elif [[ "$project_name" == "cluster2" ]]; then
        # For cluster2 project, containers are named ex-esdb10, ex-esdb11, etc.
        filter_pattern="name=^ex-esdb1[0-7]$"
    else
        # For other projects, use the standard pattern
        filter_pattern="name=${project_name}_ex-esdb"
    fi
    
    local running_containers=$(docker ps --filter "$filter_pattern" --format "{{.Names}}" | wc -l 2>/dev/null || echo "0")
    local total_containers=$(docker ps -a --filter "$filter_pattern" --format "{{.Names}}" | wc -l 2>/dev/null || echo "0")
    
    # Debug output (comment out in production)
    # echo "Debug: cluster=$cluster_name, project=$project_name, filter=$filter_pattern, running=$running_containers, total=$total_containers" >&2
    
    if [[ $running_containers -eq 0 ]] && [[ $total_containers -eq 0 ]]; then
        echo -e "${BLUE}●${NC} Not created"
    elif [[ $running_containers -eq 0 ]]; then
        echo -e "${RED}●${NC} Stopped ($total_containers containers)"
    elif [[ $running_containers -eq $total_containers ]] && [[ $total_containers -gt 0 ]]; then
        echo -e "${GREEN}●${NC} Running ($running_containers nodes)"
    else
        echo -e "${YELLOW}●${NC} Partial ($running_containers/$total_containers nodes)"
    fi
}

# Function to show cluster status
show_status() {
    print_header
    print_color $WHITE "Current Cluster Status:"
    echo
    
    for cluster in "${!CLUSTERS[@]}"; do
        local project=$(get_cluster_config $cluster "project")
        local description=$(get_cluster_config $cluster "description")
        
        printf "  %-12s %s %s\n" "$cluster" "$(get_cluster_status $cluster $project)" "($description)"
    done
    
    echo
    print_color $CYAN "Network Status:"
    if docker network ls | grep -q "ex-esdb-net"; then
        echo -e "  ex-esdb-net    ${GREEN}●${NC} Available"
    else
        echo -e "  ex-esdb-net    ${RED}●${NC} Not found"
    fi
    
    echo
    print_color $CYAN "Volume Status:"
    if [[ -d "/volume/ex-esdb" ]]; then
        local size=$(du -sh /volume/ex-esdb 2>/dev/null | cut -f1)
        echo -e "  /volume/ex-esdb    ${GREEN}●${NC} Available ($size)"
    else
        echo -e "  /volume/ex-esdb    ${RED}●${NC} Not found"
    fi
}

# Function to start a cluster
start_cluster() {
    local cluster_name=$1
    local files=$(get_cluster_config $cluster_name "files")
    local project=$(get_cluster_config $cluster_name "project")
    local description=$(get_cluster_config $cluster_name "description")
    
    print_color $YELLOW "Starting $cluster_name ($description)..."
    echo
    
    # Convert comma-separated files to -f arguments
    local compose_args=""
    IFS=',' read -ra file_array <<< "$files"
    for file in "${file_array[@]}"; do
        if [[ -f "$SCRIPT_DIR/$file" ]]; then
            compose_args="$compose_args -f $file"
        else
            print_color $RED "Error: File $file not found!"
            return 1
        fi
    done
    
    # Check if we need to create directories
    if [[ "$cluster_name" == "cluster1" ]]; then
        print_color $CYAN "Creating data directories for cluster1..."
        sudo mkdir -p /volume/ex-esdb/data{0..4}
        sudo chown "$USER" -R /volume/
    elif [[ "$cluster_name" == "cluster2" ]]; then
        print_color $CYAN "Creating data directories for cluster2..."
        sudo mkdir -p /volume/ex-esdb/data{10..17}
        sudo chown "$USER" -R /volume/
    fi
    
    # Start the cluster
    cd "$SCRIPT_DIR"
    if [[ "$cluster_name" == "cluster1" ]]; then
        docker-compose $compose_args --profile cluster -p $project up --remove-orphans --build -d
    elif [[ "$cluster_name" == "cluster2" ]]; then
        docker-compose $compose_args --profile cluster -p $project up --remove-orphans --build -d
    else
        docker-compose $compose_args -p $project up --remove-orphans --build -d
    fi
    
    if [[ $? -eq 0 ]]; then
        print_color $GREEN "✓ $cluster_name started successfully!"
    else
        print_color $RED "✗ Failed to start $cluster_name"
    fi
}

# Function to stop a cluster
stop_cluster() {
    local cluster_name=$1
    local files=$(get_cluster_config $cluster_name "files")
    local project=$(get_cluster_config $cluster_name "project")
    local description=$(get_cluster_config $cluster_name "description")
    
    print_color $YELLOW "Stopping $cluster_name ($description)..."
    echo
    
    # Convert comma-separated files to -f arguments
    local compose_args=""
    IFS=',' read -ra file_array <<< "$files"
    for file in "${file_array[@]}"; do
        if [[ -f "$SCRIPT_DIR/$file" ]]; then
            compose_args="$compose_args -f $file"
        fi
    done
    
    cd "$SCRIPT_DIR"
    if [[ "$cluster_name" == "cluster1" ]]; then
        docker-compose $compose_args --profile cluster -p $project down
    elif [[ "$cluster_name" == "cluster2" ]]; then
        docker-compose $compose_args --profile cluster -p $project down
    else
        docker-compose $compose_args -p $project down
    fi
    
    if [[ $? -eq 0 ]]; then
        print_color $GREEN "✓ $cluster_name stopped successfully!"
    else
        print_color $RED "✗ Failed to stop $cluster_name"
    fi
}

# Function to restart a cluster
restart_cluster() {
    local cluster_name=$1
    stop_cluster $cluster_name
    echo
    start_cluster $cluster_name
}

# Function to show logs
show_logs() {
    local cluster_name=$1
    local project=$(get_cluster_config $cluster_name "project")
    
    print_color $YELLOW "Showing logs for $cluster_name..."
    echo
    print_color $CYAN "Press Ctrl+C to exit log view"
    echo
    
    docker-compose -p $project logs -f
}

# Function to clean all data
clean_all_data() {
    print_color $RED "⚠️  WARNING: This will delete ALL cluster data!"
    echo
    read -p "Are you sure you want to continue? (yes/no): " confirm
    
    if [[ "$confirm" == "yes" ]]; then
        print_color $YELLOW "Stopping all clusters..."
        for cluster in "${!CLUSTERS[@]}"; do
            stop_cluster $cluster >/dev/null 2>&1
        done
        
        print_color $YELLOW "Removing data directories..."
        sudo rm -rf /volume/ex-esdb/data*
        
        print_color $YELLOW "Removing Docker volumes..."
        docker volume prune -f
        
        print_color $GREEN "✓ All data cleaned successfully!"
    else
        print_color $CYAN "Operation cancelled."
    fi
}

# Function to toggle environment mode
toggle_environment() {
    if [[ "$ENVIRONMENT_MODE" == "dev" ]]; then
        ENVIRONMENT_MODE="prod"
        print_color $GREEN "✓ Switched to Production mode"
    else
        ENVIRONMENT_MODE="dev"
        print_color $GREEN "✓ Switched to Development mode"
    fi
}

# Function to get current environment display
get_environment_display() {
    if [[ "$ENVIRONMENT_MODE" == "prod" ]]; then
        echo -e "${RED}PRODUCTION${NC}"
    else
        echo -e "${GREEN}DEVELOPMENT${NC}"
    fi
}

# Function to show resource usage
show_resource_usage() {
    print_color $YELLOW "Docker Resource Usage:"
    echo
    
    # Show running containers
    print_color $CYAN "Running Containers:"
    docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}" | grep -E "(NAMES|ex-esdb)"
    echo
    
    # Show resource consumption
    print_color $CYAN "Resource Consumption:"
    docker stats --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}" | grep -E "(CONTAINER|ex-esdb)"
    echo
    
    # Show disk usage
    print_color $CYAN "Volume Usage:"
    docker system df
}

# Enhanced start function with production support
start_cluster_enhanced() {
    local cluster_name=$1
    local files=$(get_cluster_config $cluster_name "files")
    local project=$(get_cluster_config $cluster_name "project")
    local description=$(get_cluster_config $cluster_name "description")
    
    print_color $YELLOW "Starting $cluster_name ($description) in $(get_environment_display) mode..."
    echo
    
    # Convert comma-separated files to -f arguments
    local compose_args=""
    IFS=',' read -ra file_array <<< "$files"
    for file in "${file_array[@]}"; do
        if [[ -f "$SCRIPT_DIR/$file" ]]; then
            compose_args="$compose_args -f $file"
        else
            print_color $RED "Error: File $file not found!"
            return 1
        fi
    done
    
    # Add production override if in prod mode
    if [[ "$ENVIRONMENT_MODE" == "prod" ]] && [[ -n "${PROD_OVERRIDES[$cluster_name]}" ]]; then
        local override_file="${PROD_OVERRIDES[$cluster_name]}"
        if [[ -f "$SCRIPT_DIR/$override_file" ]]; then
            compose_args="$compose_args -f $override_file"
            print_color $CYAN "Using production overrides: $override_file"
        else
            print_color $YELLOW "Warning: Production override file $override_file not found, using development config"
        fi
    fi
    
    # Check if we need to create directories
    if [[ "$cluster_name" == "cluster1" ]]; then
        print_color $CYAN "Creating data directories for cluster1..."
        sudo mkdir -p /volume/ex-esdb/data{0..4}
        sudo chown "$USER" -R /volume/
    elif [[ "$cluster_name" == "cluster2" ]]; then
        print_color $CYAN "Creating data directories for cluster2..."
        sudo mkdir -p /volume/ex-esdb/data{10..17}
        sudo chown "$USER" -R /volume/
    fi
    
    # Start the cluster
    cd "$SCRIPT_DIR"
    if [[ "$cluster_name" == "cluster1" ]] || [[ "$cluster_name" == "cluster2" ]]; then
        docker-compose $compose_args --profile cluster -p $project up --remove-orphans --build -d
    else
        docker-compose $compose_args -p $project up --remove-orphans --build -d
    fi
    
    if [[ $? -eq 0 ]]; then
        print_color $GREEN "✓ $cluster_name started successfully in $(get_environment_display) mode!"
        if [[ "$ENVIRONMENT_MODE" == "prod" ]]; then
            print_color $CYAN "Production features enabled: Enhanced monitoring, resource limits, aggressive health checks"
        fi
    else
        print_color $RED "✗ Failed to start $cluster_name"
    fi
}

# --- Chaos Engineering Functions ---

# Helper to get a random running container from any ex-esdb cluster
get_random_running_node() {
    local running_nodes
    running_nodes=($(docker ps --filter "name=^ex-esdb" --format "{{.Names}}" 2>/dev/null))
    
    if [ ${#running_nodes[@]} -eq 0 ]; then
        echo ""
    else
        # Select a random index from the array
        echo "${running_nodes[$((RANDOM % ${#running_nodes[@]}))]}"
    fi
}

kill_random_node() {
    print_color $YELLOW "🔥 Initiating chaos: Killing a random node..."
    local node_to_kill=$(get_random_running_node)
    
    if [ -z "$node_to_kill" ]; then
        print_color $RED "No running nodes to kill."
    else
        print_color $RED "💥 Killing node: $node_to_kill"
        docker kill "$node_to_kill"
        print_color $GREEN "✓ Chaos action complete."
    fi
}

stop_random_node() {
    print_color $YELLOW "🔥 Initiating chaos: Stopping a random node gracefully..."
    local node_to_stop=$(get_random_running_node)
    
    if [ -z "$node_to_stop" ]; then
        print_color $RED "No running nodes to stop."
    else
        print_color $RED "🛑 Stopping node: $node_to_stop"
        docker stop "$node_to_stop"
        print_color $GREEN "✓ Chaos action complete."
    fi
}

pause_random_node() {
    print_color $YELLOW "🔥 Initiating chaos: Pausing a random node..."
    local node_to_pause=$(get_random_running_node)
    
    if [ -z "$node_to_pause" ]; then
        print_color $RED "No running nodes to pause."
    else
        print_color $RED "⏸️ Pausing node: $node_to_pause (simulates unresponsiveness)"
        docker pause "$node_to_pause"
        print_color $GREEN "✓ Chaos action complete. The cluster should see this node as unhealthy/unreachable."
    fi
}

unpause_all_nodes() {
    print_color $YELLOW "🧊 Recovering from chaos: Unpausing all paused nodes..."
    local paused_nodes
    paused_nodes=($(docker ps --filter "status=paused" --filter "name=^ex-esdb" --format "{{.Names}}" 2>/dev/null))

    if [ ${#paused_nodes[@]} -eq 0 ]; then
        print_color $GREEN "No paused nodes found."
    else
        for node in "${paused_nodes[@]}"; do
            print_color $CYAN "▶️ Resuming node: $node"
            docker unpause "$node"
        done
        print_color $GREEN "✓ All paused nodes have been resumed."
    fi
}

# Function to show main menu
show_menu() {
    print_header
    show_status
    echo
    print_color $WHITE "Environment Mode: $(get_environment_display)"
    echo
    print_color $WHITE "Available Actions:"
    echo
    print_color $GREEN "  [s] Show Status"
    print_color $GREEN "  [u] Show Resource Usage"
    print_color $GREEN "  [e] Toggle Environment ($(get_environment_display))"
    echo
    print_color $BLUE "  [1] Start Cluster1 (nodes 0-4)"
    print_color $BLUE "  [2] Start Cluster2 (nodes 10-17)"
    print_color $BLUE "  [3] Start Swarm"
    print_color $BLUE "  [a] Start All Clusters"
    echo
    print_color $YELLOW "  [4] Stop Cluster1"
    print_color $YELLOW "  [5] Stop Cluster2"
    print_color $YELLOW "  [6] Stop Swarm"
    print_color $YELLOW "  [z] Stop All Clusters"
    echo
    print_color $PURPLE "  [r1] Restart Cluster1"
    print_color $PURPLE "  [r2] Restart Cluster2"
    print_color $PURPLE "  [r3] Restart Swarm"
    echo
    print_color $CYAN "  [l1] Show Cluster1 Logs"
    print_color $CYAN "  [l2] Show Cluster2 Logs"
    print_color $CYAN "  [l3] Show Swarm Logs"
    echo
    print_color $RED "  [🔥] === CHAOS ENGINEERING ==="
    print_color $RED "  [ck] Kill Random Node (hard crash)"
    print_color $RED "  [cs] Stop Random Node (graceful)"
    print_color $RED "  [cp] Pause Random Node (freeze)"
    print_color $GREEN "  [cr] Resume All Paused Nodes"
    echo
    print_color $RED "  [c] Clean All Data"
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
            u|U) show_resource_usage ;;
            e|E) toggle_environment ;;
            1) start_cluster_enhanced "cluster1" ;;
            2) start_cluster_enhanced "cluster2" ;;
            3) start_cluster "swarm" ;;
            a|A) 
                start_cluster_enhanced "cluster1"
                echo
                start_cluster_enhanced "cluster2"
                ;;
            4) stop_cluster "cluster1" ;;
            5) stop_cluster "cluster2" ;;
            6) stop_cluster "swarm" ;;
            z|Z) 
                for cluster in "${!CLUSTERS[@]}"; do
                    stop_cluster $cluster
                    echo
                done
                ;;
            r1|R1) 
                stop_cluster "cluster1"
                echo
                start_cluster_enhanced "cluster1"
                ;;
            r2|R2) 
                stop_cluster "cluster2"
                echo
                start_cluster_enhanced "cluster2"
                ;;
            r3|R3) restart_cluster "swarm" ;;
            l1|L1) show_logs "cluster1" ;;
            l2|L2) show_logs "cluster2" ;;
            l3|L3) show_logs "swarm" ;;
            ck|CK) kill_random_node ;;
            cs|CS) stop_random_node ;;
            cp|CP) pause_random_node ;;
            cr|CR) unpause_all_nodes ;;
            c|C) clean_all_data ;;
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
