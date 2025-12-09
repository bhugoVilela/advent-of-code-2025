#!/usr/bin/env bash

INDIR="${1:-src}"
OUTDIR="${2:-docs}"
SCRIPTDIR=$(dirname "$0")

set -e

rm -rf out
pandoc "$INDIR"/*.{md,lhs} \
    -o "$OUTDIR"/index.html \
    -f markdown+lhs \
    -t html \
    --toc=true \
    --toc-depth=2 \
    --css style.css \
    --lua-filter "$SCRIPTDIR"/filter.lua \
    --highlight-style "$SCRIPTDIR"/highlight.theme \
    --template "$SCRIPTDIR"/template.html

cp book/style.css ${OUTDIR}/style.css

echo "book generated in ./$OUTDIR"
