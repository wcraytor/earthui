#!/bin/bash
# Set terminal window title and optionally background color
# Usage: ./scripts/title.sh "My Window Title" ["dark green"|"dark blue"|"dark red"|"brown"|"purple"]

if [[ -z "$1" ]]; then
    echo "Usage: title.sh \"Window Title\" [background]"
    echo "Backgrounds: dark green, dark blue, dark red, brown, purple,"
    echo "             orange, green, red, blue, grey, dark grey"
    exit 1
fi

# Set title
printf '\033]0;%s\007' "$1"

# Set background color if specified (using AppleScript for Terminal.app)
if [[ -n "$2" ]]; then
    case "$2" in
        "dark green")
            RGB="0, 13107, 0"
            ;;
        "dark blue")
            RGB="0, 0, 17476"
            ;;
        "dark red")
            RGB="17476, 0, 0"
            ;;
        "brown")
            RGB="13107, 8738, 4369"
            ;;
        "purple")
            RGB="13107, 0, 17476"
            ;;
        "orange")
            RGB="26214, 13107, 0"
            ;;
        "green")
            RGB="0, 26214, 0"
            ;;
        "red")
            RGB="26214, 0, 0"
            ;;
        "blue")
            RGB="0, 0, 26214"
            ;;
        "grey"|"gray")
            RGB="19660, 19660, 19660"
            ;;
        "dark grey"|"dark gray")
            RGB="9830, 9830, 9830"
            ;;
        *)
            echo "Unknown background: $2"
            echo "Options: dark green, dark blue, dark red, brown, purple,"
            echo "         orange, green, red, blue, grey, dark grey"
            exit 1
            ;;
    esac

    osascript <<EOF
tell application "Terminal"
    set background color of selected tab of front window to {$RGB}
end tell
EOF
fi
