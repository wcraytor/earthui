#!/bin/bash
# ValEngr RCA - Startup Script for Linux
# Run this script to start all containers

echo "Starting ValEngr RCA containers..."
echo ""

# Navigate to the ValEngr directory
cd "$(dirname "$0")"

# Start all containers
docker-compose up -d

# Wait for containers to be ready
echo ""
echo "Waiting for services to start..."
sleep 5

# Open services in default browser
echo ""
echo "Opening RStudio..."
xdg-open http://localhost:8787 2>/dev/null || sensible-browser http://localhost:8787 2>/dev/null &

sleep 2
echo "Opening JupyterLab..."
xdg-open http://localhost:8888 2>/dev/null || sensible-browser http://localhost:8888 2>/dev/null &

sleep 2
echo "Opening VS Code..."
xdg-open http://localhost:8080 2>/dev/null || sensible-browser http://localhost:8080 2>/dev/null &

sleep 2
echo "Opening SWISH (Prolog IDE)..."
xdg-open http://localhost:3050 2>/dev/null || sensible-browser http://localhost:3050 2>/dev/null &

echo ""
echo "========================================"
echo "ValEngr RCA is ready!"
echo "========================================"
echo ""
echo "Services running:"
echo "  • RStudio:    http://localhost:8787 (username: rstudio, password: appraisal123)"
echo "  • JupyterLab: http://localhost:8888"
echo "  • VS Code:    http://localhost:8080"
echo "  • SWISH:      http://localhost:3050 (Prolog IDE)"
echo ""
echo "To stop: run ./Stop-ValEngr.sh"
echo ""
