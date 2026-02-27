#!/bin/bash
# ValEngr RCA - Stop Script for Linux
# Run this script to stop all containers

echo "Stopping ValEngr RCA containers..."
echo ""

# Navigate to the ValEngr directory
cd "$(dirname "$0")"

# Stop all containers
docker-compose down

echo ""
echo "All containers stopped."
echo ""
echo "To start again: run ./Start-ValEngr.sh"
echo ""
