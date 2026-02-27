#!/bin/bash
#
# ValEngr - API Tests Runner
#
cd "$(dirname "$0")"

echo "========================================"
echo "ValEngr - API Tests"
echo "========================================"
echo ""

# Check if API container is running
if ! docker ps | grep -q "valengr-api"; then
    echo "API container is not running"
    echo "Start containers with: ./Start-ValEngr.sh"
    exit 1
fi

# Run pytest in the API container
docker exec valengr-api bash -c 'cd /shared/tests/_shared/python && python -m pytest -v'
exit_code=$?

echo ""
echo "========================================"
if [ $exit_code -eq 0 ]; then
    echo "API Tests: PASSED"
else
    echo "API Tests: FAILED"
fi
echo "========================================"

exit $exit_code
