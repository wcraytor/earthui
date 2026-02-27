#!/bin/bash
# ValEngr RCA - Run Infrastructure Tests (Linux)

cd "$(dirname "$0")"
./tests/infrastructure/test_containers.sh
