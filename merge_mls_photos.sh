#!/bin/bash
#
# Merge mls_photos into mls_photos on Synology
#
# Moves property folders that exist in mls_photos but NOT in mls_photos.
# Since both are on the same volume, moves are instant (no data copy).
#
# Usage: ./merge_mls_photos.sh [--dry-run]
#

set -e

# Configuration
SYNOLOGY_HOST="${SYNOLOGY_HOST:-PvnNas01.local}"
SYNOLOGY_USER="${SYNOLOGY_USER:-william}"
OLD_DIR="/volume1/REData/mls_photos"
NEW_DIR="/volume1/REData/mls_photos"
LOG_FILE="/volume1/REData/mls_photo_merge.log"

# Counties to process
COUNTIES="alameda contra_costa monterey san_mateo santa_clara santa_cruz"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

DRY_RUN=false
if [[ "$1" == "--dry-run" ]]; then
    DRY_RUN=true
    echo -e "${YELLOW}DRY RUN MODE - No files will be moved${NC}"
fi

echo "========================================"
echo "MLS Photo Folder Merge Script"
echo "========================================"
echo "Source: $OLD_DIR (mls_photos)"
echo "Target: $NEW_DIR (mls_photos)"
echo ""

# Test SSH connection
echo -n "Testing SSH connection to $SYNOLOGY_HOST... "
if ssh -o ConnectTimeout=10 -o BatchMode=yes "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" 'echo OK' >/dev/null 2>&1; then
    echo -e "${GREEN}OK${NC}"
else
    echo -e "${RED}FAILED${NC}"
    echo "Cannot connect to Synology. Check SSH configuration."
    exit 1
fi

# Process each county
TOTAL_MOVED=0
TOTAL_SKIPPED=0

for county in $COUNTIES; do
    echo ""
    echo -e "${YELLOW}Processing $county...${NC}"

    # Get list of properties to move (in old but not in new)
    # Use bash explicitly since Synology default shell is sh
    MISSING=$(ssh -o BatchMode=yes "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" bash -c "'
        comm -23 \
            <(ls \"$OLD_DIR/$county\" 2>/dev/null | grep -v \"@eaDir\" | sort) \
            <(ls \"$NEW_DIR/$county\" 2>/dev/null | grep -v \"@eaDir\" | sort)
    '" 2>/dev/null || echo "")

    if [[ -z "$MISSING" ]]; then
        echo "  No properties to move"
        continue
    fi

    COUNT=$(echo "$MISSING" | wc -l | tr -d ' ')

    echo "  Found $COUNT properties to move"

    if [[ "$DRY_RUN" == "true" ]]; then
        echo "  [DRY RUN] Would move $COUNT property folders"
        # Show first 5 as examples
        echo "$MISSING" | head -5 | while read prop; do
            echo "    - $prop"
        done
        if [[ "$COUNT" -gt 5 ]]; then
            echo "    ... and $((COUNT - 5)) more"
        fi
        TOTAL_SKIPPED=$((TOTAL_SKIPPED + COUNT))
    else
        # Move properties on Synology directly
        echo "  Moving properties (this runs on Synology, instant moves)..."

        # Pass the list to Synology and move there using bash
        echo "$MISSING" | ssh -o BatchMode=yes "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" bash -c "'
            mkdir -p \"$NEW_DIR/$county\"
            echo \"\$(date): Starting merge for $county - $COUNT properties\" >> \"$LOG_FILE\"
            MOVED=0
            ERRORS=0
            while IFS= read -r prop; do
                if [[ -n \"\$prop\" ]]; then
                    if mv \"$OLD_DIR/$county/\$prop\" \"$NEW_DIR/$county/\" 2>/dev/null; then
                        MOVED=\$((MOVED + 1))
                    else
                        ERRORS=\$((ERRORS + 1))
                        echo \"  ERROR moving: \$prop\" >&2
                    fi
                fi
            done
            echo \"\$(date): Finished $county - moved \$MOVED, errors \$ERRORS\" >> \"$LOG_FILE\"
            echo \"RESULT:\$MOVED:\$ERRORS\"
        '" 2>&1 | while read line; do
            if [[ "$line" == RESULT:* ]]; then
                MOVED=$(echo "$line" | cut -d: -f2)
                ERRORS=$(echo "$line" | cut -d: -f3)
                echo -e "  ${GREEN}Moved $MOVED properties${NC}"
                if [[ "$ERRORS" -gt 0 ]]; then
                    echo -e "  ${RED}Errors: $ERRORS${NC}"
                fi
            elif [[ "$line" == *ERROR* ]]; then
                echo -e "  ${RED}$line${NC}"
            fi
        done

        TOTAL_MOVED=$((TOTAL_MOVED + COUNT))
    fi
done

echo ""
echo "========================================"
echo "Summary"
echo "========================================"
if [[ "$DRY_RUN" == "true" ]]; then
    echo -e "Dry run complete. Would move ${YELLOW}$TOTAL_SKIPPED${NC} property folders."
    echo ""
    echo "Run without --dry-run to perform the actual merge."
else
    echo -e "Total processed: ${GREEN}$TOTAL_MOVED${NC} property folders"
    echo ""
    echo "Log file on Synology: $LOG_FILE"
    echo ""
    echo "After verifying the merge, you can delete the old folder:"
    echo "  ssh ${SYNOLOGY_USER}@${SYNOLOGY_HOST} 'rm -rf $OLD_DIR'"
fi
