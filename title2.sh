#!/bin/bash
# Set terminal window title and optionally background color
# Usage: ./scripts/title.sh "My Window Title" ["dark green"|"dark blue"|"dark red"|"brown"|"purple"]


if [[ -z "$1" ]]; then
    echo 'Usage: ./title2.sh "Title" [blue|purple|green|red|grey|default]'
    exit 1
fi

title="$1"
color="$2"

# Set title
printf '\033]0;%s\007' "$title"

# Only try color changes in iTerm2
[[ "$TERM_PROGRAM" == "iTerm.app" ]] || exit 0

case "$color" in
  blue)   printf '\033]1337;SetColors=bg=000066\007' ;;
  purple) printf '\033]1337;SetColors=bg=440044\007' ;;
  green)  printf '\033]1337;SetColors=bg=003300\007' ;;
  red)    printf '\033]1337;SetColors=bg=440000\007' ;;
  grey|gray) printf '\033]1337;SetColors=bg=333333\007' ;;
   "dark green") printf '\033]1337;SetColors=bg=003300\007' ;;
  "dark blue")  printf '\033]1337;SetColors=bg=000044\007' ;;
  "dark red")   printf '\033]1337;SetColors=bg=440000\007' ;;
 
  default) printf '\033]1337;SetColors=preset=Default\007' ;;
  "") ;;
  *) echo "Unknown color: $color"; exit 1 ;;
esac
