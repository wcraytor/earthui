#!/opt/homebrew/bin/bash
# ==============================================================================
# Photo Metadata Queuing Script
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
# Queries RETS for photo URLs and queues them for download.
# Uses python/queue_all_photos.py which queries listings with photo_count > 0
# and fetches photo metadata from RETS.
#
# Usage:
#   ./run_photo_metadata.sh santa_clara              # Queue for Santa Clara
#   ./run_photo_metadata.sh 43                       # Queue by county ID
#   ./run_photo_metadata.sh --status                 # Show status for all counties
#
# To run in background:
#   nohup ./run_photo_metadata.sh santa_clara > queue_metadata.out 2>&1 &
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

# Start caffeinate to prevent sleep (will be killed when script exits)
caffeinate -i -w $$ &
CAFFEINATE_PID=$!
trap "kill $CAFFEINATE_PID 2>/dev/null" EXIT

echo "Started caffeinate (PID: $CAFFEINATE_PID) - Mac will not sleep"

# Get county argument (can be name or ID)
COUNTY="$1"
shift 2>/dev/null

if [[ -z "$COUNTY" ]]; then
    echo "Usage: $0 <county_name_or_id>"
    echo ""
    echo "Counties: alameda, contra_costa, monterey, san_mateo, santa_clara, santa_cruz"
    echo "          Or use county IDs: 1, 7, 27, 41, 43, 44"
    echo ""
    echo "Examples:"
    echo "  $0 santa_clara    # Queue photos for Santa Clara"
    echo "  $0 43             # Same, using county ID"
    echo "  $0 all            # Queue for all counties"
    exit 1
fi

# Convert county ID to name if needed (queue_all_photos.py accepts both)
# Note: queue_all_photos.py uses --counties (plural) argument
./venv/bin/python ./python/queue_all_photos.py --counties "$COUNTY" "$@"
