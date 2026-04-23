#!/usr/bin/env bash
# ==============================================================================
# earthui_layout.sh
# ==============================================================================
# Applies layout for the earthUI tmux session:
#
#   ┌──────────────────────────────────────────────────────────┐
#   │  header     (colour6   / teal,       10 lines, read-only)│
#   ├──────────────────────────────────────────────────────────┤
#   │  shell      (colour18  / dark-blue,  full width)         │
#   ├────────────────────┬────────────────────┬────────────────┤
#   │  macOS             │  Ubuntu            │  Win11         │
#   │  (colour88)        │  (colour61)        │  (colour135)   │
#   └──────────────────────────────────────────────────────────┘
#
# Called from Win11 pane: sleep 1 && .../earthui_layout.sh &
#
# Strategy (no custom layout strings):
#   1. even-vertical  → all 5 panes stacked full-width in a column
#   2. move-pane -h   → pull Ubuntu and Win11 into a row with macOS
#   3. resize-pane    → set row heights and equalize the 3 column widths
#   4. select-pane -P → apply background colors using known pane IDs
# ==============================================================================

SESSION="earthUI"
WIN="${SESSION}:main"

sleep 0.5

tmux has-session -t "$SESSION" 2>/dev/null || exit 1

# ---------------------------------------------------------------------------
# Dimensions
# ---------------------------------------------------------------------------
W=$(tmux display-message -t "$WIN" -p '#{window_width}')
H=$(tmux display-message -t "$WIN" -p '#{window_height}')

[[ "$W" =~ ^[0-9]+$ && "$H" =~ ^[0-9]+$ ]] || exit 1

HDR_H=10
# 2 dividers: header|shell, shell|OS-row
REMAINING=$(( H - HDR_H - 2 ))

if (( REMAINING < 10 )); then
  tmux select-layout -t "$WIN" tiled
  exit 0
fi

SHL_H=$(( REMAINING * 40 / 100 ))
(( SHL_H < 4 )) && SHL_H=4
OS_H=$(( REMAINING - SHL_H ))
(( OS_H < 5 )) && { OS_H=5; SHL_H=$(( REMAINING - OS_H )); }

# 3 columns: 2 dividers
BASE_C=$(( (W - 2) / 3 ))
(( BASE_C < 5 )) && { tmux select-layout -t "$WIN" tiled; exit 0; }

# ---------------------------------------------------------------------------
# Get actual pane IDs — list-panes returns them sorted by window-relative
# index.  Use mapfile (bash 4+) for 0-indexed assignment.
# Creation order: header(0) shell(1) macOS(2) Ubuntu(3) Win11(4)
# ---------------------------------------------------------------------------
PIDS=()
while IFS= read -r line; do
  PIDS+=("$line")
done < <(tmux list-panes -t "$WIN" -F '#{pane_id}')

if (( ${#PIDS[@]} < 5 )); then
  echo "earthui_layout: expected 5 panes, got ${#PIDS[@]}" >&2
  tmux select-layout -t "$WIN" tiled
  exit 1
fi

P_HDR=${PIDS[0]}
P_SHL=${PIDS[1]}
P_MAC=${PIDS[2]}
P_UBU=${PIDS[3]}
P_WIN=${PIDS[4]}

# ---------------------------------------------------------------------------
# Step 1: Stack all panes in a single column (every pane spans full width).
# ---------------------------------------------------------------------------
tmux select-layout -t "$WIN" even-vertical

# ---------------------------------------------------------------------------
# Step 2: Move Ubuntu and Win11 into the macOS row.
# After two moves: macOS | Ubuntu | Win11   (shell and header above, full-width)
# ---------------------------------------------------------------------------
tmux move-pane -h -s "$P_UBU" -t "$P_MAC"
tmux move-pane -h -s "$P_WIN" -t "$P_UBU"

# ---------------------------------------------------------------------------
# Step 3: Set column widths, then row heights top-to-bottom.
# OS row auto-fills the remainder (= REMAINING - SHL_H), no explicit resize needed.
# ---------------------------------------------------------------------------
tmux resize-pane -t "$P_MAC" -x "$BASE_C"
tmux resize-pane -t "$P_UBU" -x "$BASE_C"

tmux resize-pane -t "$P_HDR" -y "$HDR_H"
tmux resize-pane -t "$P_SHL" -y "$SHL_H"

# ---------------------------------------------------------------------------
# Step 4: Apply pane background colors using explicit pane IDs.
# (select-layout / move-pane can reset styles set earlier in the YAML.)
# ---------------------------------------------------------------------------
tmux select-pane -t "$P_HDR" -d                   # disable keyboard input (read-only)
tmux select-pane -t "$P_HDR" -P 'bg=colour98'     # teal
tmux select-pane -t "$P_SHL" -P 'bg=colour68'    # dark blue
tmux select-pane -t "$P_MAC" -P 'bg=colour88'    # dark red
tmux select-pane -t "$P_UBU" -P 'bg=colour61'    # blue-violet
tmux select-pane -t "$P_WIN" -P 'bg=colour135'   # orchid
