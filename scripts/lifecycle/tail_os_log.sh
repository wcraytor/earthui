#!/usr/bin/env bash
# Finds and tails the most recent earthUI event log for a given OS output directory.
# Usage: tail_os_log.sh <os_name>   (macos | ubuntu | win11)
# Polls every 10 s if no log exists yet; re-attaches after a log ends.
OS=${1:?Usage: tail_os_log.sh <os_name>}
LOG_DIR="/Volumes/Nvme_1/ClaudeCode/earthUI/Output/${OS}"
mkdir -p "$LOG_DIR"

while true; do
  LOGFILE=$(ls -t "$LOG_DIR/"*_earthui_log.txt 2>/dev/null | head -1)
  if [[ -n "$LOGFILE" ]]; then
    echo "==> $LOGFILE"
    tail -f "$LOGFILE"
    echo ""
    echo "Log ended — waiting for new log..."
  else
    printf '\r%s  [%s] No earthUI log yet in Output/%s' "$(date +%T)" "$OS" "$OS"
  fi
  sleep 10
done
