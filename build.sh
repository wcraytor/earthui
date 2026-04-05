#!/usr/bin/env bash
# Build script for earthUI R package
set -e

cd "$(dirname "$0")"

echo "=== Rebuilding roxygen docs + NAMESPACE ==="
Rscript -e 'roxygen2::roxygenise("pkg")'

echo ""
echo "=== Installing package locally ==="
R CMD INSTALL pkg

echo ""
echo "=== Build complete ==="
