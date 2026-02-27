#!/opt/homebrew/bin/bash
# ==============================================================================
# MLS Sync Master Script
# ==============================================================================
#
# IMPORTANT: This script runs on macOS host (not in Docker container) because:
#   1. It needs SSH access to Synology NAS for photo storage
#   2. It uses caffeinate to prevent Mac sleep during long syncs
#   3. The Python venv with RETS libraries is on the macOS host at ./venv
#   4. PostgreSQL is accessed via localhost:5432 (container port-forwarded)
#
# REQUIRES: Homebrew bash 4+ for associative arrays (declare -A)
#   - macOS default /bin/bash is version 3.2 (too old)
#   - Install: brew install bash
#   - This script uses: /opt/homebrew/bin/bash
#
# ARCHITECTURE:
#   Process 1: Listing Sync (run_listing_sync.sh)
#              - Downloads listing data from RETS
#              - Stores in PostgreSQL mls_listing table
#
#   Process 2: Photo Metadata Queuing (run_photo_metadata.sh)
#              - Queries RETS for photo URLs
#              - Queues them in mls_photos table
#
#   Process 3: Photo Downloads (run_photo_download.sh)
#              - Downloads queued photos via SSH to Synology
#              - Updates download status in mls_photos
#
# Usage:
#   ./start_mls_sync.sh                                    # Start default counties
#   ./start_mls_sync.sh --counties san_mateo,alameda       # Start specific counties
#   ./start_mls_sync.sh --counties all                     # Start all counties
#   ./start_mls_sync.sh --photos alameda,santa_clara       # Download photos for specific counties
#   ./start_mls_sync.sh --photos all                       # Download photos for all counties
#   ./start_mls_sync.sh --status                           # Check status of everything
#   ./start_mls_sync.sh --stop                             # Stop all syncs
#   ./start_mls_sync.sh --photo_status                     # Detailed photo download status
#   ./start_mls_sync.sh --restart_photos                   # Restart only photo downloads
#
# Available counties: alameda, contra_costa, monterey, san_mateo, santa_clara, santa_cruz
# ==============================================================================

cd "$(dirname "$0")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_ok()      { echo -e "${GREEN}[✓]${NC} $1"; }
print_warn()    { echo -e "${YELLOW}[!]${NC} $1"; }
print_error()   { echo -e "${RED}[✗]${NC} $1"; }
print_info()    { echo -e "${BLUE}[i]${NC} $1"; }

# Synology SSH configuration (NOT SMB - SSH is faster and more reliable)
SYNOLOGY_HOST="${SYNOLOGY_HOST:-PvnNas01.local}"
SYNOLOGY_USER="${SYNOLOGY_USER:-william}"
SYNOLOGY_PHOTO_PATH="${SYNOLOGY_PHOTO_PATH:-/volume1/REData/mls_photos}"

# SSH key - try common locations if not explicitly set
if [[ -z "$SYNOLOGY_SSH_KEY" ]]; then
    for key_path in \
        "/Users/MaxTask829/.ssh/id_ed25519" \
        "/Users/MaxTask829/.ssh/id_rsa" \
        "$HOME/.ssh/id_ed25519" \
        "$HOME/.ssh/id_rsa"; do
        if [[ -f "$key_path" ]]; then
            SYNOLOGY_SSH_KEY="$key_path"
            break
        fi
    done
fi

# Build SSH options
SSH_OPTS="-o ConnectTimeout=10 -o BatchMode=yes"
if [[ -n "$SYNOLOGY_SSH_KEY" ]]; then
    SSH_OPTS="$SSH_OPTS -i $SYNOLOGY_SSH_KEY"
fi

# Function to check SSH connectivity to Synology
check_synology_ssh() {
    ssh $SSH_OPTS "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" 'echo OK' >/dev/null 2>&1
}

# Function to get disk space via SSH
get_synology_disk_space() {
    ssh $SSH_OPTS "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" "df -h ${SYNOLOGY_PHOTO_PATH}" 2>/dev/null | tail -1 | awk '{print $4}'
}

# All available counties (name -> ID mapping)
# Usage: get_county_id "san_mateo" returns 41
get_county_id() {
    case "$1" in
        alameda)      echo 1 ;;
        contra_costa) echo 7 ;;
        monterey)     echo 27 ;;
        san_mateo)    echo 41 ;;
        santa_clara)  echo 43 ;;
        santa_cruz)   echo 44 ;;
        *) echo "" ;;
    esac
}

get_county_display_name() {
    case "$1" in
        alameda)      echo "Alameda" ;;
        contra_costa) echo "Contra Costa" ;;
        monterey)     echo "Monterey" ;;
        san_mateo)    echo "San Mateo" ;;
        santa_clara)  echo "Santa Clara" ;;
        santa_cruz)   echo "Santa Cruz" ;;
        *) echo "" ;;
    esac
}

ALL_COUNTIES="alameda contra_costa monterey san_mateo santa_clara santa_cruz"
DEFAULT_COUNTIES="san_mateo santa_clara santa_cruz"
DEFAULT_PHOTO_COUNTIES="$ALL_COUNTIES"  # All counties by default

# Parse arguments
SELECTED_COUNTIES=""
if [[ "$1" == "--counties" ]]; then
    if [[ -z "$2" ]]; then
        echo "Usage: $0 --counties county1,county2,..."
        echo "Available: alameda, contra_costa, monterey, san_mateo, santa_clara, santa_cruz"
        echo "Or use: $0 --counties all"
        exit 1
    fi
    if [[ "$2" == "all" ]]; then
        SELECTED_COUNTIES="$ALL_COUNTIES"
    else
        # Convert comma-separated to space-separated
        SELECTED_COUNTIES=$(echo "$2" | tr ',' ' ')
    fi
    # Validate counties
    for county in $SELECTED_COUNTIES; do
        if [[ -z "$(get_county_id "$county")" ]]; then
            print_error "Unknown county: $county"
            echo "Available: alameda, contra_costa, monterey, san_mateo, santa_clara, santa_cruz"
            exit 1
        fi
    done
    shift 2
fi

# Parse --photos argument
SELECTED_PHOTO_COUNTIES=""
if [[ "$1" == "--photos" ]]; then
    if [[ -z "$2" ]]; then
        echo "Usage: $0 --photos county1,county2,..."
        echo "Available: alameda, contra_costa, monterey, san_mateo, santa_clara, santa_cruz"
        echo "Or use: $0 --photos all"
        exit 1
    fi
    if [[ "$2" == "all" ]]; then
        SELECTED_PHOTO_COUNTIES="$ALL_COUNTIES"
    else
        # Convert comma-separated to space-separated
        SELECTED_PHOTO_COUNTIES=$(echo "$2" | tr ',' ' ')
    fi
    # Validate counties
    for county in $SELECTED_PHOTO_COUNTIES; do
        if [[ -z "$(get_county_id "$county")" ]]; then
            print_error "Unknown county: $county"
            echo "Available: alameda, contra_costa, monterey, san_mateo, santa_clara, santa_cruz"
            exit 1
        fi
    done
    shift 2
fi

# Parse --no-photo-downloads flag
SKIP_PHOTO_DOWNLOADS=false
if [[ "$1" == "--no-photo-downloads" ]]; then
    SKIP_PHOTO_DOWNLOADS=true
    shift
fi

# Build arrays from selected counties (or defaults)
LISTING_COUNTY_IDS=()
LISTING_COUNTY_NAMES=()

if [[ -n "$SELECTED_COUNTIES" ]]; then
    for county in $SELECTED_COUNTIES; do
        LISTING_COUNTY_IDS+=("$(get_county_id "$county")")
        LISTING_COUNTY_NAMES+=("$(get_county_display_name "$county")")
    done
else
    # Default counties if not specified and not a special command
    if [[ "$1" != "--status" && "$1" != "--stop" && "$1" != "--photo_status" && "$1" != "--restart_photos" ]]; then
        for county in $DEFAULT_COUNTIES; do
            LISTING_COUNTY_IDS+=("$(get_county_id "$county")")
            LISTING_COUNTY_NAMES+=("$(get_county_display_name "$county")")
        done
    else
        # For status/stop, include all counties to check
        for county in $ALL_COUNTIES; do
            LISTING_COUNTY_IDS+=("$(get_county_id "$county")")
            LISTING_COUNTY_NAMES+=("$(get_county_display_name "$county")")
        done
    fi
fi

# Build photo county arrays from selected or defaults
PHOTO_COUNTY_IDS=()
PHOTO_COUNTY_NAMES=()

if [[ -n "$SELECTED_PHOTO_COUNTIES" ]]; then
    for county in $SELECTED_PHOTO_COUNTIES; do
        PHOTO_COUNTY_IDS+=("$(get_county_id "$county")")
        PHOTO_COUNTY_NAMES+=("$(get_county_display_name "$county")")
    done
else
    # Default photo counties if not specified and not a special command
    if [[ "$1" != "--status" && "$1" != "--stop" && "$1" != "--photo_status" && "$1" != "--restart_photos" ]]; then
        for county in $DEFAULT_PHOTO_COUNTIES; do
            PHOTO_COUNTY_IDS+=("$(get_county_id "$county")")
            PHOTO_COUNTY_NAMES+=("$(get_county_display_name "$county")")
        done
    else
        # For status/stop, include all counties to check
        for county in $ALL_COUNTIES; do
            PHOTO_COUNTY_IDS+=("$(get_county_id "$county")")
            PHOTO_COUNTY_NAMES+=("$(get_county_display_name "$county")")
        done
    fi
fi

# ============================================================
# STATUS MODE
# ============================================================
if [[ "$1" == "--status" ]]; then
    echo ""
    echo "============================================================"
    echo "                   MLS SYNC STATUS"
    echo "============================================================"
    echo ""

    # PostgreSQL (direct connection check)
    echo "--- Infrastructure ---"
    if PGPASSWORD=appraisal_secure_2024 psql -h localhost -U valengr_admin -d mls_data -c "SELECT 1" &>/dev/null; then
        print_ok "PostgreSQL is running (localhost:5432)"
    else
        print_error "PostgreSQL is NOT accessible"
    fi

    # Caffeinate
    if pgrep -q caffeinate; then
        print_ok "caffeinate is running (sleep prevention active)"
    else
        print_warn "caffeinate is NOT running - Mac may sleep"
    fi

    # Disk space (via SSH to Synology)
    echo ""
    echo "--- Disk Space (Synology) ---"
    ssh $SSH_OPTS "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" "df -h ${SYNOLOGY_PHOTO_PATH}" 2>/dev/null || echo "  Could not check Synology disk space"

    # MLS Listing partition record counts
    echo ""
    echo "--- MLS Listing Record Counts by Partition ---"
    PGPASSWORD=appraisal_secure_2024 psql -h localhost -U valengr_admin -d mls_data -t -c "
        SELECT
            COALESCE(tableoid::regclass::text, 'TOTAL') AS partition,
            to_char(count(*), 'FM999,999,999') AS records
        FROM mls_listing
        GROUP BY ROLLUP(tableoid)
        ORDER BY
            CASE WHEN tableoid IS NULL THEN 1 ELSE 0 END,
            tableoid::regclass::text;
    " 2>/dev/null | while read -r line; do
        if [[ -n "$line" ]]; then
            # Parse partition name and count
            partition=$(echo "$line" | cut -d'|' -f1 | xargs)
            count=$(echo "$line" | cut -d'|' -f2 | xargs)
            if [[ "$partition" == "TOTAL" ]]; then
                echo ""
                printf "  ${GREEN}%-30s %s${NC}\n" "TOTAL:" "$count"
            elif [[ -n "$partition" ]]; then
                printf "  %-30s %s\n" "$partition:" "$count"
            fi
        fi
    done
    if [[ $? -ne 0 ]]; then
        print_warn "Could not query mls_listing partitions"
    fi

    # Sync Status by County (backfill complete, phase, date range)
    echo ""
    echo "--- Sync Status by County ---"
    printf "  ${BLUE}%-14s %-10s %-10s %-12s %s${NC}\n" "COUNTY" "PHASE" "BACKFILL" "LISTINGS" "DATE RANGE"
    PGPASSWORD=appraisal_secure_2024 psql -h localhost -U valengr_admin -d mls_data -t -c "
        SELECT
            s.county_name,
            s.current_phase,
            CASE WHEN s.historical_complete THEN 'Complete' ELSE 'In Progress' END,
            to_char(l.cnt, 'FM999,999'),
            to_char(s.oldest_synced_date, 'YYYY-MM-DD') || ' to ' || to_char(s.newest_synced_date, 'YYYY-MM-DD')
        FROM mls_sync_state_county s
        LEFT JOIN (SELECT county_id, count(*) as cnt FROM mls_listing GROUP BY county_id) l ON s.county_id = l.county_id
        ORDER BY s.county_id;
    " 2>/dev/null | while IFS='|' read -r county phase backfill listings daterange; do
        county=$(echo "$county" | xargs)
        phase=$(echo "$phase" | xargs)
        backfill=$(echo "$backfill" | xargs)
        listings=$(echo "$listings" | xargs)
        daterange=$(echo "$daterange" | xargs)
        if [[ -n "$county" ]]; then
            if [[ "$backfill" == "Complete" ]]; then
                printf "  %-14s %-10s ${GREEN}%-10s${NC} %-12s %s\n" "$county" "$phase" "$backfill" "$listings" "$daterange"
            else
                printf "  %-14s %-10s ${YELLOW}%-10s${NC} %-12s %s\n" "$county" "$phase" "$backfill" "$listings" "$daterange"
            fi
        fi
    done

    # Photo Status by County (queued vs downloaded)
    echo ""
    echo "--- Photo Status by County ---"
    printf "  ${BLUE}%-14s %12s %12s %12s %8s${NC}\n" "COUNTY" "QUEUED" "DOWNLOADED" "PENDING" "PCT"
    PGPASSWORD=appraisal_secure_2024 psql -h localhost -U valengr_admin -d mls_data -t -c "
        SELECT
            CASE p.county_id
                WHEN 1 THEN 'Alameda' WHEN 7 THEN 'Contra Costa' WHEN 27 THEN 'Monterey'
                WHEN 41 THEN 'San Mateo' WHEN 43 THEN 'Santa Clara' WHEN 44 THEN 'Santa Cruz'
            END,
            to_char(COUNT(*), 'FM999,999,999'),
            to_char(COUNT(*) FILTER (WHERE download_status = 'completed'), 'FM999,999,999'),
            to_char(COUNT(*) FILTER (WHERE download_status = 'pending'), 'FM999,999,999'),
            ROUND(100.0 * COUNT(*) FILTER (WHERE download_status = 'completed') / NULLIF(COUNT(*), 0), 1)::text || '%'
        FROM mls_photos p
        GROUP BY p.county_id
        ORDER BY p.county_id;
    " 2>/dev/null | while IFS='|' read -r county queued downloaded pending pct; do
        county=$(echo "$county" | xargs)
        queued=$(echo "$queued" | xargs)
        downloaded=$(echo "$downloaded" | xargs)
        pending=$(echo "$pending" | xargs)
        pct=$(echo "$pct" | xargs)
        if [[ -n "$county" ]]; then
            # Color code by completion percentage
            pct_num=${pct%\%}
            if (( $(echo "$pct_num >= 90" | bc -l 2>/dev/null || echo 0) )); then
                printf "  %-14s %12s %12s %12s ${GREEN}%8s${NC}\n" "$county" "$queued" "$downloaded" "$pending" "$pct"
            elif (( $(echo "$pct_num >= 50" | bc -l 2>/dev/null || echo 0) )); then
                printf "  %-14s %12s %12s %12s ${YELLOW}%8s${NC}\n" "$county" "$queued" "$downloaded" "$pending" "$pct"
            else
                printf "  %-14s %12s %12s %12s %8s\n" "$county" "$queued" "$downloaded" "$pending" "$pct"
            fi
        fi
    done

    # Process 1: Listing sync processes
    echo ""
    echo "--- Process 1: Listing Sync ---"
    for i in "${!LISTING_COUNTY_IDS[@]}"; do
        COUNTY_ID="${LISTING_COUNTY_IDS[$i]}"
        NAME="${LISTING_COUNTY_NAMES[$i]}"
        PID=$(pgrep -f "rets_sync_county.*--county $COUNTY_ID([^0-9]|$)" | head -1)
        if [[ -n "$PID" ]]; then
            print_ok "$NAME (County $COUNTY_ID) - PID: $PID"
        else
            print_warn "$NAME (County $COUNTY_ID) - NOT RUNNING"
        fi
    done

    # Process 2: Photo metadata queuing processes
    echo ""
    echo "--- Process 2: Photo Metadata Queuing ---"
    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        COUNTY_ID="${PHOTO_COUNTY_IDS[$i]}"
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        PID=$(pgrep -f "queue_photo_metadata.*--county $COUNTY_ID([^0-9]|$)" | head -1)
        if [[ -n "$PID" ]]; then
            print_ok "$NAME Metadata (County $COUNTY_ID) - PID: $PID"
        else
            print_warn "$NAME Metadata (County $COUNTY_ID) - NOT RUNNING"
        fi
    done

    # Process 3: Photo download processes
    echo ""
    echo "--- Process 3: Photo Downloads ---"
    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        COUNTY_ID="${PHOTO_COUNTY_IDS[$i]}"
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        FOLDER=$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]')
        PID=$(pgrep -f "download_photos.*$FOLDER" | head -1)
        if [[ -n "$PID" ]]; then
            print_ok "$NAME Photos - PID: $PID"
        else
            print_warn "$NAME Photos - NOT RUNNING"
        fi
    done

    # Recent log output
    echo ""
    echo "--- Recent Log Output ---"
    for i in "${!LISTING_COUNTY_IDS[@]}"; do
        NAME="${LISTING_COUNTY_NAMES[$i]}"
        log="sync_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"
        if [[ -f "$log" ]]; then
            echo ""
            echo "=== $log (last 3 lines) ==="
            tail -3 "$log"
        fi
    done
    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        log="metadata_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"
        if [[ -f "$log" ]]; then
            echo ""
            echo "=== $log (last 3 lines) ==="
            tail -3 "$log"
        fi
    done
    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        log="photos_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"
        if [[ -f "$log" ]]; then
            echo ""
            echo "=== $log (last 3 lines) ==="
            tail -3 "$log"
        fi
    done

    echo ""
    exit 0
fi

# ============================================================
# PHOTO STATUS MODE
# ============================================================
if [[ "$1" == "--photo_status" ]]; then
    echo ""
    echo "============================================================"
    echo "               PHOTO DOWNLOAD STATUS"
    echo "============================================================"
    echo ""

    # Check if photo download is running
    echo "--- Process Status ---"
    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        COUNTY_ID="${PHOTO_COUNTY_IDS[$i]}"
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        FOLDER=$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]')
        PID=$(pgrep -f "download_photos.*$FOLDER" | head -1)
        if [[ -n "$PID" ]]; then
            print_ok "$NAME Photos - PID: $PID (running)"
            # Show process uptime
            START_TIME=$(ps -o lstart= -p $PID 2>/dev/null)
            if [[ -n "$START_TIME" ]]; then
                echo "      Started: $START_TIME"
            fi
        else
            print_warn "$NAME Photos - NOT RUNNING"
        fi
    done

    # Show detailed photo stats from database
    echo ""
    echo "--- Database Statistics ---"
    ./run_photo_download.sh --status

    # Recent log entries
    echo ""
    echo "--- Recent Log Entries (last 20 lines) ---"
    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        log="photos_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"
        if [[ -f "$log" ]]; then
            echo ""
            echo "=== $log ==="
            tail -20 "$log"
        fi
    done
    if [[ ${#PHOTO_COUNTY_IDS[@]} -eq 0 ]] || [[ ! -f "photos_$(echo "${PHOTO_COUNTY_NAMES[0]}" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out" ]]; then
        print_warn "No log files found"
    fi

    # Check for errors in log
    echo ""
    echo "--- Recent Errors (if any) ---"
    FOUND_ERRORS=0
    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        log="photos_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"
        if [[ -f "$log" ]]; then
            ERRORS=$(grep -i -E 'error|exception|traceback|failed' "$log" | tail -5)
            if [[ -n "$ERRORS" ]]; then
                echo "=== $log ==="
                echo "$ERRORS"
                FOUND_ERRORS=1
            fi
        fi
    done
    if [[ $FOUND_ERRORS -eq 0 ]]; then
        print_ok "No recent errors found"
    fi

    echo ""
    exit 0
fi

# ============================================================
# RESTART PHOTOS MODE
# ============================================================
if [[ "$1" == "--restart_photos" ]]; then
    # If no --photos was specified before --restart_photos, use defaults
    if [[ ${#PHOTO_COUNTY_IDS[@]} -eq 0 ]]; then
        for county in $DEFAULT_PHOTO_COUNTIES; do
            PHOTO_COUNTY_IDS+=("$(get_county_id "$county")")
            PHOTO_COUNTY_NAMES+=("$(get_county_display_name "$county")")
        done
    fi

    echo ""
    echo "============================================================"
    echo "            RESTARTING PHOTO DOWNLOADS"
    echo "============================================================"
    echo ""

    # Pre-flight checks
    echo "--- Pre-flight Checks ---"
    if PGPASSWORD=appraisal_secure_2024 psql -h localhost -U valengr_admin -d mls_data -c "SELECT 1" &>/dev/null; then
        print_ok "PostgreSQL is running (localhost:5432)"
    else
        print_error "PostgreSQL is NOT accessible on localhost:5432"
        echo ""
        echo "Please ensure PostgreSQL is running, then try again."
        exit 1
    fi

    # Check Python venv exists
    if [[ ! -f "./venv/bin/python" ]]; then
        print_error "Python venv not found at ./venv"
        echo ""
        echo "Create it with:"
        echo "  python3 -m venv venv"
        echo "  ./venv/bin/pip install -r requirements.txt"
        exit 1
    fi
    print_ok "Python venv found"

    # Stop existing photo downloads
    echo ""
    echo "--- Stopping Existing Photo Downloads ---"
    PHOTO_PIDS=$(pgrep -f "download_photos" 2>/dev/null)
    if [[ -n "$PHOTO_PIDS" ]]; then
        print_warn "Killing existing photo download processes..."
        pkill -f "download_photos"
        sleep 2
        # Force kill if still running
        if pgrep -f "download_photos" > /dev/null 2>&1; then
            pkill -9 -f "download_photos"
            sleep 1
        fi
        print_ok "Stopped existing photo downloads"
    else
        print_info "No existing photo downloads running"
    fi

    # Start photo downloads
    echo ""
    echo "--- Starting Photo Downloads ---"
    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        COUNTY_ID="${PHOTO_COUNTY_IDS[$i]}"
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        FOLDER=$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]')
        LOGFILE="photos_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"

        nohup ./run_photo_download.sh "$FOLDER" > "$LOGFILE" 2>&1 &
        PID=$!
        disown
        sleep 1

        if ps -p $PID > /dev/null 2>&1; then
            print_ok "$NAME Photos started - PID: $PID"
            print_info "  Log: $LOGFILE"
        else
            print_error "Failed to start $NAME Photos"
        fi
    done

    echo ""
    echo "Use './start_mls_sync.sh --photo_status' to check status"
    echo ""
    exit 0
fi

# ============================================================
# STOP MODE
# ============================================================
if [[ "$1" == "--stop" ]]; then
    echo ""
    echo "Stopping all MLS sync processes..."
    echo ""

    # Stop listing syncs
    for i in "${!LISTING_COUNTY_IDS[@]}"; do
        COUNTY_ID="${LISTING_COUNTY_IDS[$i]}"
        NAME="${LISTING_COUNTY_NAMES[$i]}"
        if pkill -f "rets_sync_county.*--county $COUNTY_ID([^0-9]|$)" 2>/dev/null; then
            print_ok "Stopped $NAME listing sync"
        else
            print_info "$NAME listing sync was not running"
        fi
    done

    # Stop photo downloads
    if pkill -f "download_photos" 2>/dev/null; then
        print_ok "Stopped photo downloads"
    else
        print_info "Photo downloads were not running"
    fi

    # Stop caffeinate
    if pkill caffeinate 2>/dev/null; then
        print_ok "Stopped caffeinate (sleep prevention)"
    else
        print_info "caffeinate was not running"
    fi

    echo ""
    exit 0
fi

# ============================================================
# START MODE (default)
# ============================================================
echo ""
echo "============================================================"
echo "                   MLS SYNC STARTUP"
echo "============================================================"
echo ""

# ------------------------------------------------------------
# Pre-flight checks
# ------------------------------------------------------------
echo "--- Pre-flight Checks ---"
echo ""

# 1. Check PostgreSQL (direct connection - no Docker required)
if PGPASSWORD=appraisal_secure_2024 psql -h localhost -U valengr_admin -d mls_data -c "SELECT 1" &>/dev/null; then
    print_ok "PostgreSQL is running (localhost:5432)"
else
    print_error "PostgreSQL is NOT accessible on localhost:5432"
    echo ""
    echo "Please ensure PostgreSQL is running, then try again."
    exit 1
fi

# 2. Check/start caffeinate
if pgrep -q caffeinate; then
    print_ok "caffeinate already running"
else
    caffeinate -s &
    disown
    sleep 1
    if pgrep -q caffeinate; then
        print_ok "caffeinate started (prevents Mac sleep)"
    else
        print_warn "Failed to start caffeinate - Mac may sleep"
    fi
fi

# 3. Check Synology SSH connection (used for photo downloads)
if check_synology_ssh; then
    print_ok "Synology SSH connection OK (${SYNOLOGY_USER}@${SYNOLOGY_HOST})"
else
    print_error "Cannot connect to Synology via SSH"
    echo ""
    echo "Photo downloads require SSH access to Synology."
    echo "Please check:"
    echo "  1. Synology is powered on and on the network"
    echo "  2. SSH is enabled on Synology (Control Panel > Terminal & SNMP)"
    echo "  3. Test connection: ssh ${SYNOLOGY_USER}@${SYNOLOGY_HOST} 'echo OK'"
    echo ""
    echo "If SSH key is not configured, run:"
    echo "  ssh-copy-id ${SYNOLOGY_USER}@${SYNOLOGY_HOST}"
    exit 1
fi

# 4. Check disk space via SSH
AVAIL=$(get_synology_disk_space)
if [[ -n "$AVAIL" ]]; then
    print_ok "Synology disk space: $AVAIL available"
else
    print_warn "Could not check Synology disk space"
fi

# 5. Check Python venv exists
if [[ ! -f "./venv/bin/python" ]]; then
    print_error "Python venv not found at ./venv"
    echo ""
    echo "Create it with:"
    echo "  python3 -m venv venv"
    echo "  ./venv/bin/pip install -r requirements.txt"
    exit 1
fi
print_ok "Python venv found"

# ------------------------------------------------------------
# Kill duplicate processes
# ------------------------------------------------------------
echo ""
echo "--- Ensuring Single Instances ---"
echo ""

for i in "${!LISTING_COUNTY_IDS[@]}"; do
    COUNTY_ID="${LISTING_COUNTY_IDS[$i]}"
    NAME="${LISTING_COUNTY_NAMES[$i]}"
    EXISTING=$(pgrep -f "rets_sync_county.*--county $COUNTY_ID([^0-9]|$)" | wc -l | tr -d ' ')
    if [[ "$EXISTING" -gt 0 ]]; then
        print_warn "Killing $EXISTING existing $NAME listing sync process(es)"
        pkill -f "rets_sync_county.*--county $COUNTY_ID([^0-9]|$)"
        sleep 1
    else
        print_ok "No existing $NAME listing sync"
    fi
done

PHOTO_EXISTING=$(pgrep -f "download_photos" | wc -l | tr -d ' ')
if [[ "$PHOTO_EXISTING" -gt 0 ]]; then
    print_warn "Killing $PHOTO_EXISTING existing photo download process(es)"
    pkill -f "download_photos"
    sleep 1
else
    print_ok "No existing photo downloads"
fi

# ------------------------------------------------------------
# Start listing syncs (Process 1: listings only, no photo metadata)
# ------------------------------------------------------------
echo ""
echo "--- Starting Listing Syncs (no photo metadata) ---"
echo ""

for i in "${!LISTING_COUNTY_IDS[@]}"; do
    COUNTY_ID="${LISTING_COUNTY_IDS[$i]}"
    NAME="${LISTING_COUNTY_NAMES[$i]}"
    LOGFILE="sync_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"

    nohup ./run_listing_sync.sh "$COUNTY_ID" > "$LOGFILE" 2>&1 &
    PID=$!
    disown
    sleep 1

    if ps -p $PID > /dev/null 2>&1; then
        print_ok "$NAME Listings (County $COUNTY_ID) started - PID: $PID"
        print_info "  Log: $LOGFILE"
    else
        print_error "Failed to start $NAME Listings (County $COUNTY_ID)"
    fi
done

# ------------------------------------------------------------
# Start photo metadata queuing (Process 2: queue photo URLs)
# ------------------------------------------------------------
echo ""
echo "--- Starting Photo Metadata Queuing ---"
echo ""

for i in "${!PHOTO_COUNTY_IDS[@]}"; do
    COUNTY_ID="${PHOTO_COUNTY_IDS[$i]}"
    NAME="${PHOTO_COUNTY_NAMES[$i]}"
    LOGFILE="metadata_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"

    nohup ./run_photo_metadata.sh "$COUNTY_ID" > "$LOGFILE" 2>&1 &
    PID=$!
    disown
    sleep 1

    if ps -p $PID > /dev/null 2>&1; then
        print_ok "$NAME Metadata (County $COUNTY_ID) started - PID: $PID"
        print_info "  Log: $LOGFILE"
    else
        print_error "Failed to start $NAME Metadata (County $COUNTY_ID)"
    fi
done

# ------------------------------------------------------------
# Start photo downloads (Process 3: download actual photo files)
# ------------------------------------------------------------
if [[ "$SKIP_PHOTO_DOWNLOADS" == true ]]; then
    echo ""
    echo "--- Skipping Photo Downloads (--no-photo-downloads) ---"
    echo ""
else
    echo ""
    echo "--- Starting Photo Downloads ---"
    echo ""

    for i in "${!PHOTO_COUNTY_IDS[@]}"; do
        COUNTY_ID="${PHOTO_COUNTY_IDS[$i]}"
        NAME="${PHOTO_COUNTY_NAMES[$i]}"
        FOLDER=$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]')
        LOGFILE="photos_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"

        nohup ./run_photo_download.sh "$FOLDER" > "$LOGFILE" 2>&1 &
        PID=$!
        disown
        sleep 1

        if ps -p $PID > /dev/null 2>&1; then
            print_ok "$NAME Photos started - PID: $PID"
            print_info "  Log: $LOGFILE"
        else
            print_error "Failed to start $NAME Photos"
        fi
    done
fi

# ------------------------------------------------------------
# Summary
# ------------------------------------------------------------
echo ""
echo "============================================================"
echo "                      SUMMARY"
echo "============================================================"
echo ""
echo "Running processes:"
ps aux | grep -E 'rets_sync_county|queue_photo_metadata|download_photos' | grep -v grep | awk '{printf "  PID: %-6s %s %s %s\n", $2, $11, $12, $13}'

echo ""
echo "Log files:"
for i in "${!LISTING_COUNTY_IDS[@]}"; do
    NAME="${LISTING_COUNTY_NAMES[$i]}"
    LOGFILE="sync_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"
    printf "  %-30s - %s listings\n" "$LOGFILE" "$NAME"
done
for i in "${!PHOTO_COUNTY_IDS[@]}"; do
    NAME="${PHOTO_COUNTY_NAMES[$i]}"
    LOGFILE="metadata_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"
    printf "  %-30s - %s photo metadata\n" "$LOGFILE" "$NAME"
done
for i in "${!PHOTO_COUNTY_IDS[@]}"; do
    NAME="${PHOTO_COUNTY_NAMES[$i]}"
    LOGFILE="photos_$(echo "$NAME" | tr ' ' '_' | tr '[:upper:]' '[:lower:]').out"
    printf "  %-30s - %s photo downloads\n" "$LOGFILE" "$NAME"
done

echo ""
echo "Commands:"
echo "  ./start_mls_sync.sh --status                    # Check status"
echo "  ./start_mls_sync.sh --stop                      # Stop all"
echo "  ./start_mls_sync.sh --counties san_mateo,alameda # Start specific counties"
echo "  ./start_mls_sync.sh --counties all              # Start all counties"
echo "  ./start_mls_sync.sh --no-photo-downloads        # Start listings + metadata only (no downloads)"
echo "  ./start_mls_sync.sh --photos alameda,santa_clara # Download photos for specific counties"
echo "  ./start_mls_sync.sh --photos all                # Download photos for all counties"
echo "  ./start_mls_sync.sh --photo_status              # Detailed photo status"
echo "  ./start_mls_sync.sh --restart_photos            # Restart only photos"
echo "  tail -f sync_*.out metadata_*.out photos_*.out  # Monitor logs"
echo ""
