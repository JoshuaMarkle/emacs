#!/usr/bin/env bash
# Render every promo/src/*.html page to a 1920x1080 PNG in promo/
set -euo pipefail
cd "$(dirname "$0")"

for page in src/*.html; do
    name="$(basename "$page" .html)"
    chromium --headless=new --disable-gpu --hide-scrollbars \
        --force-device-scale-factor=1 --window-size=1920,1080 \
        --virtual-time-budget=2000 \
        --screenshot="$PWD/$name.png" "file://$PWD/$page" 2>/dev/null
    echo "rendered $name.png"
done
