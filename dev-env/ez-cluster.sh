#!/bin/bash

# ExESDB Manager Hub
# Simple launcher for specialized cluster management tools

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
    print_styled "$CYAN$BOLD" "║                            ExESDB Manager Hub                                 ║"
    print_styled "$CYAN$BOLD" "║                                                                               ║"
    print_styled "$CYAN$BOLD" "║                        Choose Your Management Tool                            ║"
    print_styled "$CYAN$BOLD" "╚═══════════════════════════════════════════════════════════════════════════════╝"
    echo
}

# Function to show main menu
show_menu() {
    clear
    print_header
    
    print_styled "$WHITE$BOLD" "🛠️  Available Management Tools:"
    echo
    
    print_styled "$GREEN" "     [1] 🌱 Manage 'Procure Supplies' Cluster"
    print_styled "$DIM" "         → Advanced proc-sup cluster management with chaos engineering"
    echo
    
    print_styled "$YELLOW" "     [2] 🌿 Manage 'Regulate Greenhouse' Cluster"
    print_styled "$DIM" "         → Specialized reg-gh cluster management"
    echo
    
    print_styled "$WHITE$BOLD" "     [q] 👋 Quit"
    echo
    
    print_styled "$DIM" "💡 Each manager provides specialized cluster management capabilities"
    echo
}

# Function to launch manager scripts
launch_manager() {
    local script_name=$1
    local script_path="$SCRIPT_DIR/$script_name"
    
    if [[ ! -f "$script_path" ]]; then
        print_styled "$RED" "❌ Manager script not found: $script_name"
        return 1
    fi
    
    print_styled "$CYAN$BOLD" "🚀 Launching $script_name..."
    echo
    
    # Set environment variable to indicate this script was called from ez-cluster
    export EZ_CLUSTER_PARENT=1
    
    # Launch the manager script
    cd "$SCRIPT_DIR"
    "$script_path"
    
    # Clear the environment variable
    unset EZ_CLUSTER_PARENT
    
    print_styled "$BLUE" "📋 Returned from $script_name"
}

# Function to handle user input
handle_input() {
    local choice=$1
    
    case $choice in
        1) launch_manager "proc-sup-cluster-manager.sh" ;;
        2) launch_manager "reg-gh-cluster-manager.sh" ;;
        q|Q) 
            print_styled "$GREEN$BOLD" "👋 Thanks for using the Manager Hub!"
            exit 0
            ;;
        *)
            print_styled "$RED" "❌ Invalid choice: '$choice'"
            print_styled "$YELLOW" "💡 Try: 1-2 or q"
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
        echo -n "$(print_styled "$WHITE$BOLD" "🎯 Select a manager: ")"
        read -r choice
        echo

        if [[ "$choice" == "q" || "$choice" == "Q" ]]; then
            print_styled "$GREEN$BOLD" "👋 Thanks for using the Manager Hub!"
            exit 0
        else
            handle_input "$choice"
        fi
    done
}

# Main entry point
if [[ $# -eq 0 ]]; then
    # Interactive mode
    main_interactive
else
    # Direct manager launch
    case $1 in
        1|procure|proc-sup) launch_manager "proc-sup-cluster-manager.sh" ;;
        2|regulate|reg-gh) launch_manager "reg-gh-cluster-manager.sh" ;;
        *)
            print_styled "$CYAN$BOLD" "ExESDB Manager Hub"
            echo
            print_styled "$WHITE" "Usage: $0 [manager]"
            echo
            print_styled "$WHITE" "Managers:"
            print_styled "$GREEN" "  1, procure       - Manage 'Procure Supplies' Cluster"
            print_styled "$YELLOW" "  2, regulate      - Manage 'Regulate Greenhouse' Cluster"
            echo
            print_styled "$CYAN" "Examples:"
            print_styled "$DIM" "  $0 1             # Launch procure supplies manager"
            print_styled "$DIM" "  $0 procure       # Launch procure supplies manager"
            print_styled "$DIM" "  $0 regulate      # Launch regulate greenhouse manager"
            print_styled "$DIM" "  $0               # Interactive menu"
            echo
            ;;
    esac
fi
