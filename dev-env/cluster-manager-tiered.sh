#!/bin/bash

set -e

show_usage() {
    echo "ExESDB Tiered Cluster Manager"
    echo ""
    echo "Usage: $0 [COMMAND] [TIER]"
    echo ""
    echo "Commands:"
    echo "  start       Start cluster tier(s)"
    echo "  stop        Stop cluster tier(s)"
    echo "  restart     Restart cluster tier(s)"
    echo "  status      Show cluster status"
    echo "  help        Show this help"
    echo ""
    echo "Tiers:"
    echo "  core        Core cluster (nodes 0, 1, 2)"
    echo "  extended    Extended tier (nodes 10, 11) - standalone"
    echo "  massive     Massive tier (nodes 20-27) - standalone"
    echo "  full        All tiers combined (13 nodes total)"
    echo "  all         Same as 'full'"
    echo ""
    echo "Examples:"
    echo "  $0 start core           # Start only core cluster"
    echo "  $0 start extended       # Start only extended tier"
    echo "  $0 start full           # Start all tiers (core → extended → massive)"
    echo "  $0 stop massive         # Stop only massive tier"
    echo "  $0 stop all             # Stop all tiers"
    echo "  $0 status               # Show status of all tiers"
    echo ""
    echo "Node Layout:"
    echo "  Core:     ex-esdb0, ex-esdb1, ex-esdb2 (3 nodes)"
    echo "  Extended: ex-esdb10, ex-esdb11 (2 nodes)"
    echo "  Massive:  ex-esdb20-ex-esdb27 (8 nodes)"
    echo "  Total:    13 nodes when all tiers are running"
}

show_status() {
    echo "=== ExESDB Cluster Status ==="
    echo ""
    
    # Core cluster
    echo "Core Cluster (nodes 0, 1, 2):"
    if docker-compose -f ex-esdb-cluster.yaml -p cluster ps --quiet > /dev/null 2>&1; then
        docker-compose -f ex-esdb-cluster.yaml -p cluster ps --format "table {{.Name}}\t{{.Status}}" | grep -E "(NAME|ex-esdb[0-2])" || echo "  No core nodes running"
    else
        echo "  No core nodes running"
    fi
    echo ""
    
    # Extended tier
    echo "Extended Tier (nodes 10, 11):"
    if docker-compose -f ex-esdb-cluster2.yaml -p cluster2 ps --quiet > /dev/null 2>&1; then
        docker-compose -f ex-esdb-cluster2.yaml -p cluster2 ps --format "table {{.Name}}\t{{.Status}}" | grep -E "(NAME|ex-esdb1[0-1])" || echo "  No extended nodes running"
    else
        echo "  No extended nodes running"
    fi
    echo ""
    
    # Massive tier
    echo "Massive Tier (nodes 20-27):"
    if docker-compose -f ex-esdb-cluster3.yaml -p cluster3 ps --quiet > /dev/null 2>&1; then
        docker-compose -f ex-esdb-cluster3.yaml -p cluster3 ps --format "table {{.Name}}\t{{.Status}}" | grep -E "(NAME|ex-esdb2[0-7])" || echo "  No massive nodes running"
    else
        echo "  No massive nodes running"
    fi
}

case "${1:-help}" in
    "start")
        case "${2:-help}" in
            "core")
                ./start-core-only.sh
                ;;
            "extended")
                ./start-extended-only.sh
                ;;
            "massive")
                ./start-massive-only.sh
                ;;
            "full"|"all")
                ./start-massive.sh  # This starts everything in sequence
                ;;
            *)
                echo "Error: Specify a tier to start (core, extended, massive, full)"
                echo "Use '$0 help' for usage information"
                exit 1
                ;;
        esac
        ;;
    "stop")
        case "${2:-help}" in
            "core")
                ./stop-core.sh
                ;;
            "extended")
                ./stop-extended.sh
                ;;
            "massive")
                ./stop-massive.sh
                ;;
            "all"|"full")
                ./stop-all.sh
                ;;
            *)
                echo "Error: Specify a tier to stop (core, extended, massive, all)"
                echo "Use '$0 help' for usage information"
                exit 1
                ;;
        esac
        ;;
    "restart")
        case "${2:-help}" in
            "core")
                ./stop-core.sh
                ./start-core-only.sh
                ;;
            "extended")
                ./stop-extended.sh
                ./start-extended-only.sh
                ;;
            "massive")
                ./stop-massive.sh
                ./start-massive-only.sh
                ;;
            "all"|"full")
                ./stop-all.sh
                sleep 5
                ./start-massive.sh
                ;;
            *)
                echo "Error: Specify a tier to restart (core, extended, massive, all)"
                echo "Use '$0 help' for usage information"
                exit 1
                ;;
        esac
        ;;
    "status")
        show_status
        ;;
    "help"|*)
        show_usage
        ;;
esac
