#!/bin/bash
# build_project.sh - Full build of ValEngr project
#
# IMPORTANT: All development runs inside Linux Docker containers.
# NEVER run npm, ng, or python commands directly on the host machine.
# The node_modules contain Linux binaries that will not work on macOS/Windows.
#
# USAGE:
#   ./build_project.sh [OPTIONS]
#
# OPTIONS:
#   --frontend-only   Only build the Angular frontend
#   --backend-only    Only install Python dependencies
#   --containers-only Only build Docker containers
#   --no-containers   Skip Docker container build
#   --help, -h        Show help message
#
# DESCRIPTION:
#   Builds the ValEngr project by running builds inside Docker containers.
#   This ensures the correct platform binaries are used (Linux arm64).
#
# TO APPLY CODE CHANGES WITHOUT FULL REBUILD:
#   ./start_project.sh --restart-frontend   # Apply TypeScript/Angular changes
#   ./start_project.sh --restart-backend    # Apply Python/FastAPI changes
#   ./start_project.sh --restart-all        # Apply both

set -e

# Track elapsed time
SECONDS=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get project directory
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONTAINER_PREFIX=$(basename "$PROJECT_DIR" | tr '[:upper:]' '[:lower:]' | tr '_' '-')

# Parse arguments
BUILD_FRONTEND=true
BUILD_BACKEND=true
BUILD_CONTAINERS=true

for arg in "$@"; do
    case $arg in
        --frontend-only)
            BUILD_BACKEND=false
            BUILD_CONTAINERS=false
            ;;
        --backend-only)
            BUILD_FRONTEND=false
            BUILD_CONTAINERS=false
            ;;
        --containers-only)
            BUILD_FRONTEND=false
            BUILD_BACKEND=false
            ;;
        --no-containers)
            BUILD_CONTAINERS=false
            ;;
        --help|-h)
            echo "Usage: ./build_project.sh [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --frontend-only   Only build the Angular frontend"
            echo "  --backend-only    Only install Python dependencies"
            echo "  --containers-only Only build Docker containers"
            echo "  --no-containers   Skip Docker container build"
            echo "  --help, -h        Show this help message"
            exit 0
            ;;
    esac
done

echo -e "${BLUE}=========================================${NC}"
echo -e "${BLUE}Building ValEngr Project${NC}"
echo -e "${BLUE}=========================================${NC}"

# Check if containers are running
ANGULAR_CONTAINER="${CONTAINER_PREFIX}-angular"
API_CONTAINER="${CONTAINER_PREFIX}-api"

containers_running() {
    docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_PREFIX}-"
}

# Build Docker containers first if needed
if [ "$BUILD_CONTAINERS" = true ]; then
    echo ""
    echo -e "${YELLOW}1. Building Docker containers...${NC}"
    cd "$PROJECT_DIR"
    docker-compose build
    echo -e "   ${GREEN}✓ Docker containers built${NC}"
fi

# Start containers if not running (needed for in-container builds)
if ! containers_running; then
    echo ""
    echo -e "${YELLOW}Starting containers for build...${NC}"

    # Source existing ports or use defaults
    if [ -f "$PROJECT_DIR/.active_ports" ]; then
        source "$PROJECT_DIR/.active_ports"
    else
        export CONTAINER_PREFIX
        export DB_NAME=$(echo "$PROJECT_DIR" | xargs basename | tr '[:upper:]' '[:lower:]' | tr '-' '_')_db
    fi

    cd "$PROJECT_DIR"
    docker-compose up -d

    # Wait for containers to be ready
    echo -e "${YELLOW}Waiting for containers to start...${NC}"
    sleep 5
fi

# Frontend build (inside container)
if [ "$BUILD_FRONTEND" = true ]; then
    echo ""
    echo -e "${YELLOW}2. Building Angular frontend (inside container)...${NC}"

    # Check if angular container exists
    if docker ps --format '{{.Names}}' | grep -q "^${ANGULAR_CONTAINER}$"; then
        # Install dependencies and build
        echo -e "   Installing npm dependencies..."
        docker exec "$ANGULAR_CONTAINER" npm install

        echo -e "   Building production bundle..."
        docker exec "$ANGULAR_CONTAINER" npm run build -- --configuration=production

        echo -e "   ${GREEN}✓ Frontend built${NC}"
    else
        echo -e "   ${RED}✗ Angular container not running. Start with ./start_project.sh first${NC}"
        exit 1
    fi
fi

# Backend dependencies (inside container)
if [ "$BUILD_BACKEND" = true ]; then
    echo ""
    echo -e "${YELLOW}3. Installing Python dependencies (inside container)...${NC}"

    if docker ps --format '{{.Names}}' | grep -q "^${API_CONTAINER}$"; then
        docker exec "$API_CONTAINER" pip install -r /app/requirements.txt -q
        echo -e "   ${GREEN}✓ Python dependencies installed${NC}"
    else
        echo -e "   ${RED}✗ API container not running. Start with ./start_project.sh first${NC}"
        exit 1
    fi
fi

echo ""
echo -e "${GREEN}=========================================${NC}"
echo -e "${GREEN}✓ Build complete!${NC}"
echo -e "${GREEN}=========================================${NC}"
echo ""
echo -e "To start/restart: ${YELLOW}./start_project.sh${NC}"
echo -e "To rebuild containers: ${YELLOW}./start_project.sh --rebuild${NC}"
echo -e "To view logs: ${YELLOW}docker-compose logs -f angular${NC}"
echo ""

# Print elapsed time
elapsed=$SECONDS
minutes=$((elapsed / 60))
seconds=$((elapsed % 60))
if [ $minutes -gt 0 ]; then
    echo -e "Elapsed time: ${BLUE}${minutes}m ${seconds}s${NC}"
else
    echo -e "Elapsed time: ${BLUE}${seconds}s${NC}"
fi
