#!/opt/homebrew/bin/bash
# ==============================================================================
# MLS Listing Sync Script
# ==============================================================================
#
# IMPORTANT: This script runs on macOS host (not in Docker container) because:
#   1. It needs SSH access to Synology NAS for photo storage
#   2. It uses caffeinate to prevent Mac sleep during long syncs
#   3. The Python venv is on the macOS host at ./venv
#
# REQUIRES: Homebrew bash 4+ for associative arrays (declare -A)
#   - macOS default /bin/bash is version 3.2 (too old)
#   - Install: brew install bash
#   - This script uses: /opt/homebrew/bin/bash
#
# Syncs listing data only, without downloading photos.
# Photo downloads are handled separately by run_photo_download.sh
#
# Usage:
#   ./run_listing_sync.sh santa_clara              # Sync Santa Clara continuously
#   ./run_listing_sync.sh 43 --once                # Single sync cycle
#   ./run_listing_sync.sh --counties all           # All counties
#   ./run_listing_sync.sh --counties all --parallel # All counties in parallel
#   ./run_listing_sync.sh --status                 # Show status
#
# To run in background:
#   nohup ./run_listing_sync.sh santa_clara > sync_listings.out 2>&1 &
#
# ==============================================================================

cd "$(dirname "$0")"

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

# Start caffeinate to prevent sleep (will be killed when script exits)
caffeinate -i -w $$ &
CAFFEINATE_PID=$!
trap "kill $CAFFEINATE_PID 2>/dev/null" EXIT

echo "Started caffeinate (PID: $CAFFEINATE_PID) - Mac will not sleep"

# Counties mapping
declare -A COUNTY_IDS=(
    ["alameda"]=1
    ["contra_costa"]=7
    ["monterey"]=27
    ["san_mateo"]=41
    ["santa_clara"]=43
    ["santa_cruz"]=44
)

# Check for --counties flag (multi-county mode)
if [[ "$1" == "--counties" ]]; then
    shift
    COUNTIES_ARG="$1"
    shift

    # Pass through to run_county_sync.sh (--no-photos skips photo downloads)
    ./run_county_sync.sh --counties "$COUNTIES_ARG" --no-photos "$@"
elif [[ "$1" == "--status" ]]; then
    ./venv/bin/python ./python/rets_sync_county.py --status
elif [[ "$1" == "--list" ]]; then
    ./venv/bin/python ./python/rets_sync_county.py --list
else
    # Single county mode
    COUNTY="$1"
    shift

    # Convert county name to ID if needed
    if [[ -n "${COUNTY_IDS[$COUNTY]}" ]]; then
        COUNTY_ID="${COUNTY_IDS[$COUNTY]}"
    elif [[ "$COUNTY" =~ ^[0-9]+$ ]]; then
        COUNTY_ID="$COUNTY"
    else
        echo "Error: Unknown county '$COUNTY'"
        echo "Valid counties: ${!COUNTY_IDS[*]}"
        exit 1
    fi

    # --no-photos: Skip photo downloads (listing sync only)
    ./venv/bin/python ./python/rets_sync_county.py --county "$COUNTY_ID" --no-photos "$@"
fi
