#!/opt/homebrew/bin/bash
# MLS Photo Download Script
# Downloads pending photos from the mls_photos queue via SSH/rsync to Synology
#
# IMPORTANT: Uses SSH/rsync to Synology - NEVER SMB
#
# Usage:
#   ./run_photo_download.sh --counties <counties|all> [options]
#   ./run_photo_download.sh <county> [options]         # Legacy single-county mode
#   ./run_photo_download.sh --status                   # Show download status
#   ./run_photo_download.sh --stop                     # Stop running downloads
#   ./run_photo_download.sh --verify-ssh               # Test SSH connection
#   ./run_photo_download.sh --help                     # Show all options
#
# Parameters (bash wrapper):
#   --counties <list|all>   Counties to download (space-separated or "all")
#                           Examples: --counties all
#                                     --counties san_mateo
#                                     --counties "san_mateo santa_clara"
#
# Parameters (passed to Python script):
#   <county_folder>         County folder name (positional, legacy mode)
#   --county <id>           County ID (alternative to folder name)
#   --workers <n>           Number of parallel download workers (default: 5)
#   --batch <n>             Photos per batch (default: 500)
#   --limit <n>             Stop after N photos (default: run until complete)
#   --status                Show download status only, don't download
#   --stop                  Stop any running download_photos.py processes
#   --verify-ssh            Verify SSH connection to Synology and exit
#   --create-sentinel       Create sentinel file on Synology via SSH
#
# County folders:
#   alameda, contra_costa, monterey, san_mateo, santa_clara, santa_cruz
#
# Environment variables:
#   SYNOLOGY_HOST           Synology IP/hostname (default: 192.168.2.66)
#   SYNOLOGY_USER           Synology SSH user (default: william)
#   SYNOLOGY_PHOTO_PATH     Remote photo path (default: /volume1/REData/mls_photos)
#   SYNOLOGY_SSH_KEY        Path to SSH private key (auto-detected if not set)
#   MLS_PHOTO_WORKERS       Default worker count (default: 5)
#   RETS_DB_HOST            Database host (default: localhost)
#   RETS_DB_PORT            Database port (default: 5432)
#   RETS_DB_NAME            Database name (default: mls_data)
#
# Examples:
#   ./run_photo_download.sh --counties all             # Download all counties (parallel)
#   ./run_photo_download.sh --counties san_mateo       # Download San Mateo only
#   ./run_photo_download.sh san_mateo --workers 10     # Single county, 10 workers
#   ./run_photo_download.sh --status                   # Show status only
#   ./run_photo_download.sh --verify-ssh               # Test SSH connection
#
# Background execution:
#   nohup ./run_photo_download.sh --counties all > photos_all.out 2>&1 &
#
# Graceful stop:
#   Press Ctrl+C (waits for current batch to finish)
#   Or: kill -TERM <pid>
#   Or: ./run_photo_download.sh --stop

cd "$(dirname "$0")"

# Set environment variables
export RETS_PASSWORD="${RETS_PASSWORD:-BrkWcray25}"
export RETS_DB_USER="${RETS_DB_USER:-valengr_admin}"
export RETS_DB_PASSWORD="${RETS_DB_PASSWORD:-appraisal_secure_2024}"
export RETS_DB_NAME="${RETS_DB_NAME:-mls_data}"
export RETS_DB_HOST="${RETS_DB_HOST:-localhost}"
export RETS_DB_PORT="${RETS_DB_PORT:-5432}"

# Synology SSH configuration (NOT SMB)
export SYNOLOGY_HOST="${SYNOLOGY_HOST:-192.168.2.66}"
export SYNOLOGY_USER="${SYNOLOGY_USER:-william}"
export SYNOLOGY_PHOTO_PATH="${SYNOLOGY_PHOTO_PATH:-/volume1/REData/mls_photos}"

# SSH key - try common locations if not explicitly set
# This ensures the script works even when HOME is set differently (e.g., in Claude Code)
if [[ -z "$SYNOLOGY_SSH_KEY" ]]; then
    for key_path in \
        "/Users/MaxTask829/.ssh/id_ed25519" \
        "/Users/MaxTask829/.ssh/id_rsa" \
        "$HOME/.ssh/id_ed25519" \
        "$HOME/.ssh/id_rsa"; do
        if [[ -f "$key_path" ]]; then
            export SYNOLOGY_SSH_KEY="$key_path"
            break
        fi
    done
fi

# Build SSH options
SSH_OPTS="-o ConnectTimeout=10 -o BatchMode=yes"
if [[ -n "$SYNOLOGY_SSH_KEY" ]]; then
    SSH_OPTS="$SSH_OPTS -i $SYNOLOGY_SSH_KEY"
fi

# All available counties
ALL_COUNTIES="alameda contra_costa monterey san_mateo santa_clara santa_cruz"

# Function to check SSH connectivity to Synology
check_synology_ssh() {
    ssh $SSH_OPTS "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" 'echo OK' >/dev/null 2>&1
}

# Function to ensure Synology SSH connection is healthy
ensure_synology_connection() {
    local max_attempts=3
    local wait_seconds=5

    # First check if SSH connection works
    if check_synology_ssh; then
        echo "Synology SSH connection verified: ${SYNOLOGY_USER}@${SYNOLOGY_HOST}"
        [[ -n "$SYNOLOGY_SSH_KEY" ]] && echo "  Using SSH key: $SYNOLOGY_SSH_KEY"
        return 0
    fi

    # Not connected - attempt to reconnect
    echo "Synology SSH connection failed. Attempting to reconnect..."

    for attempt in $(seq 1 $max_attempts); do
        echo "Connection attempt $attempt of $max_attempts..."

        # Wait and retry
        sleep $wait_seconds

        if check_synology_ssh; then
            echo "Successfully connected to Synology via SSH"
            return 0
        fi

        if [ $attempt -lt $max_attempts ]; then
            echo "SSH connection not available, retrying..."
        fi
    done

    # All attempts failed
    echo ""
    echo "ERROR: Failed to connect to Synology via SSH after $max_attempts attempts"
    echo ""
    echo "Please check:"
    echo "  1. Synology is powered on and on the network"
    echo "  2. SSH is enabled on Synology (Control Panel > Terminal & SNMP)"
    echo "  3. SSH key authentication is set up:"
    echo "     ssh ${SYNOLOGY_USER}@${SYNOLOGY_HOST} 'echo OK'"
    echo ""
    echo "  If SSH key is not configured, run:"
    echo "     ssh-copy-id ${SYNOLOGY_USER}@${SYNOLOGY_HOST}"
    return 1
}

# Function to run download for a single county
run_county_download() {
    local county="$1"
    shift
    local extra_args="$@"

    echo "Starting download for $county..."
    ./venv/bin/python ./scripts/download_photos.py "$county" $extra_args
}

# Function to run downloads for multiple counties in parallel
run_parallel_downloads() {
    local counties="$1"
    shift
    local extra_args="$@"
    local pids=()

    echo "Starting parallel downloads for: $counties"
    echo ""

    for county in $counties; do
        echo "Launching $county download in background..."
        ./venv/bin/python ./scripts/download_photos.py "$county" $extra_args > "photos_${county}.out" 2>&1 &
        pids+=($!)
        sleep 1  # Stagger starts slightly
    done

    echo ""
    echo "All downloads started. PIDs: ${pids[*]}"
    echo ""
    echo "Monitor with:"
    echo "  tail -f photos_*.out"
    echo ""
    echo "Or check status with:"
    echo "  ./run_photo_download.sh --status"
    echo ""

    # Wait for all background processes
    echo "Waiting for all downloads to complete..."
    for pid in "${pids[@]}"; do
        wait $pid
    done

    echo "All downloads completed."
}

# Parse --counties argument
COUNTIES=""
EXTRA_ARGS=""
PARSE_COUNTIES=false

for arg in "$@"; do
    if [[ "$arg" == "--counties" ]]; then
        PARSE_COUNTIES=true
    elif [[ "$PARSE_COUNTIES" == true ]]; then
        if [[ "$arg" == "all" ]]; then
            COUNTIES="$ALL_COUNTIES"
        else
            COUNTIES="$arg"
        fi
        PARSE_COUNTIES=false
    elif [[ "$arg" == "--status" ]] || [[ "$arg" == "--help" ]] || [[ "$arg" == "--stop" ]] || [[ "$arg" == "--verify-ssh" ]] || [[ "$arg" == "--create-sentinel" ]]; then
        # Pass through to Python script without SSH check
        ./venv/bin/python ./scripts/download_photos.py "$@"
        exit $?
    else
        EXTRA_ARGS="$EXTRA_ARGS $arg"
    fi
done

# If no --counties specified, check for legacy positional argument
if [[ -z "$COUNTIES" ]] && [[ -n "$1" ]] && [[ "$1" != "--"* ]]; then
    # Legacy mode: first argument is county name
    COUNTIES="$1"
    shift
    EXTRA_ARGS="$@"
fi

# Ensure Synology SSH connection is up and healthy
if ! ensure_synology_connection; then
    exit 1
fi

# Start caffeinate to prevent sleep (will be killed when script exits)
caffeinate -i -w $$ &
CAFFEINATE_PID=$!
trap "kill $CAFFEINATE_PID 2>/dev/null" EXIT

echo "Started caffeinate (PID: $CAFFEINATE_PID) - Mac will not sleep"

# Run the downloads
if [[ -z "$COUNTIES" ]]; then
    # No counties specified - show usage
    echo ""
    echo "Usage: $0 --counties <counties|all> [options]"
    echo "       $0 <county> [options]"
    echo "       $0 --status"
    echo ""
    echo "Counties: $ALL_COUNTIES"
    echo ""
    echo "Examples:"
    echo "  $0 --counties all              # Download all counties in parallel"
    echo "  $0 --counties san_mateo        # Download San Mateo only"
    echo "  $0 --counties \"san_mateo santa_clara\"  # Multiple specific counties"
    echo "  $0 san_mateo --workers 10      # Legacy: single county with options"
    echo "  $0 --status                    # Show download status"
    exit 1
fi

# Count number of counties
COUNTY_COUNT=$(echo $COUNTIES | wc -w | tr -d ' ')

if [[ $COUNTY_COUNT -eq 1 ]]; then
    # Single county - run directly
    run_county_download "$COUNTIES" $EXTRA_ARGS
else
    # Multiple counties - run in parallel
    run_parallel_downloads "$COUNTIES" $EXTRA_ARGS
fi
