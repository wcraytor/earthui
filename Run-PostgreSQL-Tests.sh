#!/bin/bash
# ValEngr RCA - Run PostgreSQL Tests (Linux)
cd "$(dirname "$0")"
./tests/infrastructure/test_postgres.sh
