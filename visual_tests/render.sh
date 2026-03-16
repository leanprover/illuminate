#!/bin/sh
# Reads SVG from stdin, renders to PNG via Inkscape, writes PNG to stdout.
cat > /tmp/input.svg
inkscape /tmp/input.svg --export-type=png --export-filename=/tmp/out.png --export-dpi=384 2>/dev/null
cat /tmp/out.png
