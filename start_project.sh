#!/bin/bash
# start_project.sh - Start ValEngr project with dynamic ports and unique container names
#
# IMPORTANT: All development runs inside Linux Docker containers.
# NEVER run npm, ng, or python commands directly on the host machine.
# The node_modules contain Linux binaries that will not work on macOS/Windows.
#
# USAGE:
#   ./start_project.sh [OPTIONS]
#
# OPTIONS:
#   --rebuild          Rebuild containers using Docker cache (fast if no Dockerfile changes)
#   --rebuild-no-cache Rebuild containers from scratch, no cache (slow, reinstalls all R packages)
#   --build            Build Angular frontend after starting (runs npm install && npm run build inside container)
#   --stop             Stop containers and exit (runs docker-compose down)
#   --restart-frontend Restart Angular container to apply frontend code changes
#   --restart-backend  Restart API container to apply backend code changes
#   --restart-all      Restart both frontend and backend containers
#   --help, -h         Show help message
#
# EXAMPLES:
#   ./start_project.sh              # Start normally with dynamic ports
#   ./start_project.sh --build      # Start and build Angular frontend
#   ./start_project.sh --rebuild    # Rebuild containers (fast, uses cache)
#   ./start_project.sh --rebuild-no-cache  # Full rebuild (slow, no cache)
#   ./start_project.sh --stop       # Stop and remove all containers
#   ./start_project.sh --stop && ./start_project.sh   # Restart fresh
#
# DESCRIPTION:
#   This script starts the ValEngr development environment with Docker containers.
#   It automatically finds available ports to avoid conflicts with other projects.
#   Port assignments are saved to .active_ports for use by other scripts.
#
# CONTAINERS STARTED:
#   - Angular frontend (default port 4200)
#   - FastAPI backend (default port 8000)
#   - Forms API (default port 8100)
#   - PostgreSQL database (default port 5432)
#   - RStudio Server (default port 8787)
#   - JupyterLab (default port 8888)
#   - Prolog server (default port 3050)
#   - VS Code Server (default port 8080)
#   - C++ container (default port 9000)
#
# FILES GENERATED:
#   .active_ports - Shell script with exported port variables (source this file)

set -e

# Track elapsed time
SECONDS=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get project directory and name
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_NAME="$(basename "$PROJECT_DIR")"

# Convert project name to lowercase with dashes for Docker (e.g., V20251208_CA_Pacifica_243CliftonRd -> v20251208-ca-pacifica-243cliftonrd)
CONTAINER_PREFIX=$(echo "$PROJECT_NAME" | tr '[:upper:]' '[:lower:]' | tr '_' '-')

# Convert to underscore version for database name (PostgreSQL prefers underscores)
DB_NAME=$(echo "$PROJECT_NAME" | tr '[:upper:]' '[:lower:]' | tr '-' '_')_db

# Network name (project-specific)
NETWORK_NAME="${CONTAINER_PREFIX}-network"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}ValEngr Project Startup${NC}"
echo -e "${BLUE}========================================${NC}"
echo -e "Project: ${GREEN}$PROJECT_NAME${NC}"
echo -e "Container Prefix: ${GREEN}$CONTAINER_PREFIX${NC}"
echo -e "Database Name: ${GREEN}$DB_NAME${NC}"
echo -e "Network: ${GREEN}$NETWORK_NAME${NC}"
echo ""

# Handle command line arguments
REBUILD=false
NO_CACHE=false
STOP_ONLY=false
BUILD_FRONTEND=false
RESTART_FRONTEND=false
RESTART_BACKEND=false

for arg in "$@"; do
    case $arg in
        --rebuild)
            REBUILD=true
            shift
            ;;
        --rebuild-no-cache)
            REBUILD=true
            NO_CACHE=true
            shift
            ;;
        --build)
            BUILD_FRONTEND=true
            shift
            ;;
        --stop)
            STOP_ONLY=true
            shift
            ;;
        --restart-frontend)
            RESTART_FRONTEND=true
            shift
            ;;
        --restart-backend)
            RESTART_BACKEND=true
            shift
            ;;
        --restart-all)
            RESTART_FRONTEND=true
            RESTART_BACKEND=true
            shift
            ;;
        --help|-h)
            echo "Usage: ./start_project.sh [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --rebuild          Rebuild containers (uses Docker cache)"
            echo "  --rebuild-no-cache Rebuild containers from scratch (slow, reinstalls everything)"
            echo "  --build            Build Angular frontend after starting"
            echo "  --stop             Stop containers and exit"
            echo "  --restart-frontend Restart Angular container (apply frontend code changes)"
            echo "  --restart-backend  Restart API container (apply backend code changes)"
            echo "  --restart-all      Restart both frontend and backend containers"
            echo "  --help, -h         Show this help message"
            echo ""
            echo "IMPORTANT: All code runs inside Linux Docker containers."
            echo "NEVER run npm, ng, or python commands directly on the host."
            exit 0
            ;;
    esac
done

# Function to check if port is in use
check_port() {
    if command -v lsof &> /dev/null; then
        lsof -i :$1 >/dev/null 2>&1
        return $?
    elif command -v netstat &> /dev/null; then
        netstat -tuln | grep -q ":$1 "
        return $?
    else
        # Fallback: try to connect
        (echo >/dev/tcp/localhost/$1) 2>/dev/null
        return $?
    fi
}

# Function to find next available port
find_available_port() {
    local port=$1
    local max_attempts=100
    local attempt=0
    
    while check_port $port && [ $attempt -lt $max_attempts ]; do
        ((port++))
        ((attempt++))
    done
    
    if [ $attempt -eq $max_attempts ]; then
        echo -e "${RED}Error: Could not find available port starting from $1${NC}" >&2
        exit 1
    fi
    
    echo $port
}

# Stop containers if requested
if [ "$STOP_ONLY" = true ]; then
    echo -e "${YELLOW}Stopping containers...${NC}"
    
    # Export variables for docker-compose
    export CONTAINER_PREFIX
    export DB_NAME
    export NETWORK_NAME
    
    # Source existing ports if available
    if [ -f "$PROJECT_DIR/.active_ports" ]; then
        source "$PROJECT_DIR/.active_ports"
    fi
    
    cd "$PROJECT_DIR"
    docker-compose down
    
    echo -e "${GREEN}Containers stopped.${NC}"
    exit 0
fi

# Handle restart options (quick restart without full startup)
if [ "$RESTART_FRONTEND" = true ] || [ "$RESTART_BACKEND" = true ]; then
    echo -e "${YELLOW}Restarting containers to apply code changes...${NC}"

    if [ "$RESTART_FRONTEND" = true ]; then
        echo -e "  Restarting ${CONTAINER_PREFIX}-angular..."
        docker restart "${CONTAINER_PREFIX}-angular"
        echo -e "  ${GREEN}✓ Frontend container restarted${NC}"
        echo -e "  ${YELLOW}Wait ~10 seconds for Angular to recompile, then refresh browser${NC}"
    fi

    if [ "$RESTART_BACKEND" = true ]; then
        echo -e "  Restarting ${CONTAINER_PREFIX}-api..."
        docker restart "${CONTAINER_PREFIX}-api"
        echo -e "  ${GREEN}✓ Backend container restarted${NC}"
    fi

    echo ""
    echo -e "${GREEN}Restart complete!${NC}"
    echo -e "Check logs: ${YELLOW}docker logs ${CONTAINER_PREFIX}-angular --tail 30${NC}"
    exit 0
fi

# Find available ports
echo -e "${YELLOW}Finding available ports...${NC}"

ANGULAR_PORT=$(find_available_port 4200)
API_PORT=$(find_available_port 8000)
FORMS_PORT=$(find_available_port 8100)
POSTGRES_PORT=$(find_available_port 5432)
RSTUDIO_PORT=$(find_available_port 8787)
JUPYTER_PORT=$(find_available_port 8888)
PROLOG_PORT=$(find_available_port 3050)
VSCODE_PORT=$(find_available_port 8080)
CPP_PORT=$(find_available_port 9000)

echo -e "  Angular:    ${GREEN}$ANGULAR_PORT${NC}"
echo -e "  API:        ${GREEN}$API_PORT${NC}"
echo -e "  Forms:      ${GREEN}$FORMS_PORT${NC}"
echo -e "  PostgreSQL: ${GREEN}$POSTGRES_PORT${NC}"
echo -e "  RStudio:    ${GREEN}$RSTUDIO_PORT${NC}"
echo -e "  Jupyter:    ${GREEN}$JUPYTER_PORT${NC}"
echo -e "  Prolog:     ${GREEN}$PROLOG_PORT${NC}"
echo -e "  VS Code:    ${GREEN}$VSCODE_PORT${NC}"
echo ""

# Export environment variables for docker-compose
export CONTAINER_PREFIX
export DB_NAME
export NETWORK_NAME
export ANGULAR_PORT
export API_PORT
export FORMS_PORT
export POSTGRES_PORT
export RSTUDIO_PORT
export JUPYTER_PORT
export PROLOG_PORT
export VSCODE_PORT
export CPP_PORT

# Database connection info (for internal container use)
DB_HOST="${CONTAINER_PREFIX}-postgres"
DB_USER="valengr_admin"
DB_PASSWORD="appraisal_secure_2024"

export DB_HOST
export DB_USER
export DB_PASSWORD

# Write active ports to file for other scripts to source
cat > "$PROJECT_DIR/.active_ports" << EOF
# Active ports for $PROJECT_NAME
# Generated: $(date)
# Source this file: source .active_ports

export CONTAINER_PREFIX="$CONTAINER_PREFIX"
export DB_NAME="$DB_NAME"
export NETWORK_NAME="$NETWORK_NAME"

# External ports (host -> container)
export ANGULAR_PORT=$ANGULAR_PORT
export API_PORT=$API_PORT
export FORMS_PORT=$FORMS_PORT
export POSTGRES_PORT=$POSTGRES_PORT
export RSTUDIO_PORT=$RSTUDIO_PORT
export JUPYTER_PORT=$JUPYTER_PORT
export PROLOG_PORT=$PROLOG_PORT
export VSCODE_PORT=$VSCODE_PORT
export CPP_PORT=$CPP_PORT

# Database connection info
export DB_HOST="$DB_HOST"
export DB_USER="$DB_USER"
export DB_PASSWORD="$DB_PASSWORD"

# Container names
export POSTGRES_CONTAINER="${CONTAINER_PREFIX}-postgres"
export API_CONTAINER="${CONTAINER_PREFIX}-api"
export FORMS_CONTAINER="${CONTAINER_PREFIX}-forms"
export ANGULAR_CONTAINER="${CONTAINER_PREFIX}-angular"
export RSTUDIO_CONTAINER="${CONTAINER_PREFIX}-rstudio"
export PYTHON_CONTAINER="${CONTAINER_PREFIX}-python"
export PROLOG_CONTAINER="${CONTAINER_PREFIX}-prolog"
export VSCODE_CONTAINER="${CONTAINER_PREFIX}-vscode"
export CPP_CONTAINER="${CONTAINER_PREFIX}-cpp"

# URLs for browser access
export ANGULAR_URL="http://localhost:$ANGULAR_PORT"
export API_URL="http://localhost:$API_PORT"
export FORMS_URL="http://localhost:$FORMS_PORT"
export RSTUDIO_URL="http://localhost:$RSTUDIO_PORT"
export JUPYTER_URL="http://localhost:$JUPYTER_PORT"
export VSCODE_URL="http://localhost:$VSCODE_PORT"
EOF

echo -e "${YELLOW}Starting containers...${NC}"
cd "$PROJECT_DIR"

# Build and start
if [ "$REBUILD" = true ]; then
    if [ "$NO_CACHE" = true ]; then
        echo -e "${YELLOW}Rebuilding containers WITHOUT cache (this will take a long time)...${NC}"
        docker-compose build --no-cache
    else
        echo -e "${YELLOW}Rebuilding containers (using Docker cache)...${NC}"
        docker-compose build
    fi
fi

docker-compose up -d

# Wait for PostgreSQL to be healthy
echo -e "${YELLOW}Waiting for PostgreSQL to be ready...${NC}"
MAX_WAIT=60
WAITED=0
while [ $WAITED -lt $MAX_WAIT ]; do
    if docker exec "${CONTAINER_PREFIX}-postgres" pg_isready -U valengr_admin >/dev/null 2>&1; then
        echo -e "${GREEN}PostgreSQL is ready!${NC}"
        break
    fi
    sleep 2
    ((WAITED+=2))
    echo -n "."
done

if [ $WAITED -ge $MAX_WAIT ]; then
    echo -e "${RED}Warning: PostgreSQL may not be fully ready${NC}"
fi

# Build Angular frontend if requested
if [ "$BUILD_FRONTEND" = true ]; then
    echo ""
    echo -e "${YELLOW}Building Angular frontend (inside container)...${NC}"

    ANGULAR_CONTAINER="${CONTAINER_PREFIX}-angular"

    # Wait for Angular container to be ready
    sleep 3

    if docker ps --format '{{.Names}}' | grep -q "^${ANGULAR_CONTAINER}$"; then
        echo -e "  Installing npm dependencies..."
        docker exec "$ANGULAR_CONTAINER" npm install

        echo -e "  Building production bundle..."
        docker exec "$ANGULAR_CONTAINER" npm run build -- --configuration=production

        echo -e "${GREEN}  ✓ Frontend built successfully${NC}"
    else
        echo -e "${RED}  ✗ Angular container not running${NC}"
    fi
fi

# Print summary
echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Project Started Successfully!${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo -e "${BLUE}Access URLs:${NC}"
echo -e "  Angular Frontend: ${GREEN}http://localhost:$ANGULAR_PORT${NC}"
echo -e "  FastAPI Backend:  ${GREEN}http://localhost:$API_PORT${NC}"
echo -e "  API Docs:         ${GREEN}http://localhost:$API_PORT/docs${NC}"
echo -e "  Forms API:        ${GREEN}http://localhost:$FORMS_PORT${NC}"
echo -e "  Forms Docs:       ${GREEN}http://localhost:$FORMS_PORT/docs${NC}"
echo -e "  RStudio:          ${GREEN}http://localhost:$RSTUDIO_PORT${NC} (user: rstudio, pass: appraisal123)"
echo -e "  JupyterLab:       ${GREEN}http://localhost:$JUPYTER_PORT${NC}"
echo -e "  VS Code Server:   ${GREEN}http://localhost:$VSCODE_PORT${NC}"
echo ""
echo -e "${BLUE}Database:${NC}"
echo -e "  Host: ${GREEN}localhost:$POSTGRES_PORT${NC}"
echo -e "  Database: ${GREEN}$DB_NAME${NC}"
echo -e "  User: ${GREEN}$DB_USER${NC}"
echo -e "  Password: ${GREEN}$DB_PASSWORD${NC}"
echo ""
echo -e "${BLUE}Container Names:${NC}"
echo -e "  ${CONTAINER_PREFIX}-postgres"
echo -e "  ${CONTAINER_PREFIX}-api"
echo -e "  ${CONTAINER_PREFIX}-forms"
echo -e "  ${CONTAINER_PREFIX}-angular"
echo -e "  ${CONTAINER_PREFIX}-rstudio"
echo -e "  ${CONTAINER_PREFIX}-python"
echo -e "  ${CONTAINER_PREFIX}-prolog"
echo -e "  ${CONTAINER_PREFIX}-vscode"
echo ""
echo -e "${BLUE}Commands:${NC}"
echo -e "  Stop:             ${YELLOW}./start_project.sh --stop${NC}"
echo -e "  Rebuild:          ${YELLOW}./start_project.sh --rebuild${NC}"
echo -e "  Build frontend:   ${YELLOW}./start_project.sh --build${NC}"
echo -e "  Restart frontend: ${YELLOW}./start_project.sh --restart-frontend${NC}  (apply TypeScript changes)"
echo -e "  Restart backend:  ${YELLOW}./start_project.sh --restart-backend${NC}   (apply Python changes)"
echo -e "  Restart both:     ${YELLOW}./start_project.sh --restart-all${NC}"
echo -e "  Logs:             ${YELLOW}docker-compose logs -f${NC}"
echo -e "  Status:           ${YELLOW}docker-compose ps${NC}"
echo -e "  Frontend logs:    ${YELLOW}docker-compose logs -f angular${NC}"
echo ""
echo -e "Port configuration saved to: ${GREEN}.active_ports${NC}"
echo -e "Source it in scripts: ${YELLOW}source .active_ports${NC}"
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

