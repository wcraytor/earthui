#!/bin/bash
# Multi-County RETS Sync Script
#
# Usage:
#   ./run_county_sync.sh santa_clara           # Sync Santa Clara continuously (by name)
#   ./run_county_sync.sh 43                    # Sync Santa Clara continuously (by ID)
#   ./run_county_sync.sh santa_clara --once    # Single sync cycle then exit
#   ./run_county_sync.sh santa_clara --status  # Show status
#   ./run_county_sync.sh santa_clara --photos  # Photos only
#   ./run_county_sync.sh --counties santa_clara,monterey,alameda  # Multiple counties continuously
#   ./run_county_sync.sh --counties all                           # Sync all counties continuously
#   ./run_county_sync.sh --counties all --parallel                # Sync all counties in parallel
#   ./run_county_sync.sh --counties santa_clara,monterey --status # Status for multiple
#   ./run_county_sync.sh --status              # Status for all counties
#   ./run_county_sync.sh --list                # List counties
#
# Environment variables:
#   SLEEP_BETWEEN_CYCLES - Seconds to sleep between cycles (default: 100)
#
# Counties (name or ID):
#   alameda       (1)
#   contra_costa  (7)
#   monterey      (27)
#   san_mateo     (41)
#   santa_clara   (43)
#   santa_cruz    (44)
#
# To run in background:
#   nohup ./run_county_sync.sh santa_clara > sync_santa_clara.out 2>&1 &

cd "$(dirname "$0")"

# Convert county name to ID
county_name_to_id() {
    case "$1" in
        alameda)      echo 1 ;;
        contra_costa) echo 7 ;;
        monterey)     echo 27 ;;
        san_mateo)    echo 41 ;;
        santa_clara)  echo 43 ;;
        santa_cruz)   echo 44 ;;
        *)            echo "" ;;
    esac
}

# Set environment variables
export RETS_PASSWORD="${RETS_PASSWORD:-BrkWcray25}"
export RETS_DB_USER="${RETS_DB_USER:-valengr_admin}"
export RETS_DB_PASSWORD="${RETS_DB_PASSWORD:-appraisal_secure_2024}"
export RETS_DB_NAME="${RETS_DB_NAME:-mls_data}"
export RETS_DB_HOST="${RETS_DB_HOST:-localhost}"
export RETS_DB_PORT="${RETS_DB_PORT:-5432}"
export MLS_PHOTO_DIR="${MLS_PHOTO_DIR:-/Volumes/REData/mls_photos}"

# Verify Synology REData is mounted (not just a local folder)
if ! mount | grep -q "on /Volumes/REData "; then
    echo "ERROR: Synology REData is not mounted at /Volumes/REData"
    echo ""
    echo "The drive may have disconnected. To reconnect:"
    echo "  open 'smb://william@PvnNas01.local/REData'"
    echo ""
    echo "Or check mount status with: mount | grep REData"
    exit 1
fi
echo "Synology REData verified mounted at /Volumes/REData"

# Sleep between cycles (seconds)
SLEEP_BETWEEN_CYCLES=${SLEEP_BETWEEN_CYCLES:-100}

# Start caffeinate to prevent sleep (will be killed when script exits)
caffeinate -i -w $$ &
CAFFEINATE_PID=$!
trap "kill $CAFFEINATE_PID 2>/dev/null" EXIT
echo "Started caffeinate (PID: $CAFFEINATE_PID) - Mac will not sleep"

# Check for county argument (name or ID)
if [[ "$1" == "--counties" ]]; then
    # Multiple counties (comma-separated) or "all"
    shift
    COUNTIES_LIST="$1"
    shift

    # Check for --parallel flag
    PARALLEL=false
    REMAINING_ARGS=()
    for arg in "$@"; do
        if [[ "$arg" == "--parallel" ]]; then
            PARALLEL=true
        else
            REMAINING_ARGS+=("$arg")
        fi
    done
    set -- "${REMAINING_ARGS[@]}"

    # Handle "all" to expand to all counties
    if [[ "$COUNTIES_LIST" == "all" ]]; then
        COUNTIES_LIST="alameda,contra_costa,monterey,san_mateo,santa_clara,santa_cruz"
    fi
    IFS=',' read -ra COUNTY_ARRAY <<< "$COUNTIES_LIST"

    # Create logs directory for parallel mode
    LOGS_DIR="./logs"
    mkdir -p "$LOGS_DIR"

    # Continuous loop through all counties
    CYCLE=1
    while true; do
        echo ""
        echo "#################### CYCLE $CYCLE - $(date) ####################"

        if [[ "$PARALLEL" == true ]]; then
            echo "Running ${#COUNTY_ARRAY[@]} counties in PARALLEL mode"
            echo "Logs: $LOGS_DIR/sync_<county>.log"
            echo ""

            PIDS=()
            for county in "${COUNTY_ARRAY[@]}"; do
                # Convert name to ID if needed
                if [[ "$county" =~ ^[0-9]+$ ]]; then
                    COUNTY_ID="$county"
                else
                    COUNTY_ID=$(county_name_to_id "$county")
                fi
                if [[ -z "$COUNTY_ID" ]]; then
                    echo "Error: Unknown county '$county'"
                    continue
                fi

                LOG_FILE="$LOGS_DIR/sync_${county}.log"
                echo "Starting $county (ID: $COUNTY_ID) -> $LOG_FILE"
                ./venv/bin/python ./python/rets_sync_county.py --county "$COUNTY_ID" --once "$@" > "$LOG_FILE" 2>&1 &
                PIDS+=($!)
            done

            echo ""
            echo "Waiting for ${#PIDS[@]} processes to complete..."
            for pid in "${PIDS[@]}"; do
                wait "$pid"
            done
            echo "All counties completed"

        else
            # Sequential mode
            for county in "${COUNTY_ARRAY[@]}"; do
                # Convert name to ID if needed
                if [[ "$county" =~ ^[0-9]+$ ]]; then
                    COUNTY_ID="$county"
                else
                    COUNTY_ID=$(county_name_to_id "$county")
                fi
                if [[ -z "$COUNTY_ID" ]]; then
                    echo "Error: Unknown county '$county'"
                    continue
                fi
                echo ""
                echo "========== Processing county: $county (ID: $COUNTY_ID) =========="
                ./venv/bin/python ./python/rets_sync_county.py --county "$COUNTY_ID" --once "$@"
            done
        fi

        CYCLE=$((CYCLE + 1))
        echo ""
        echo "Cycle complete. Sleeping ${SLEEP_BETWEEN_CYCLES}s before next cycle..."
        echo "Press Ctrl-C to stop"
        sleep "$SLEEP_BETWEEN_CYCLES"
    done
elif [[ "$1" =~ ^[0-9]+$ ]]; then
    # Numeric county ID
    COUNTY_ID=$1
    shift
    ./venv/bin/python ./python/rets_sync_county.py --county "$COUNTY_ID" "$@"
elif [[ -n "$(county_name_to_id "$1")" ]]; then
    # County name
    COUNTY_ID=$(county_name_to_id "$1")
    shift
    ./venv/bin/python ./python/rets_sync_county.py --county "$COUNTY_ID" "$@"
else
    ./venv/bin/python ./python/rets_sync_county.py "$@"
fi
