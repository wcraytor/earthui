#!/usr/bin/env bash
# Launch earthUI Shiny app on port 7878
set -e

# Kill any existing process on port 7878
lsof -ti:7878 | xargs kill 2>/dev/null || true

Rscript -e 'earthUI::launch()'
