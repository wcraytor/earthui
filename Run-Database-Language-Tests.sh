#!/bin/bash
cd "$(dirname "$0")"

echo "========================================"
echo "ValEngr - Database Language Tests"
echo "========================================"
echo ""

# Check if PostgreSQL container is running
if ! docker ps | grep -q "valengr-postgres"; then
    echo "❌ PostgreSQL container is not running"
    echo "Start containers with: ./Start-ValEngr.sh"
    exit 1
fi

# Test counters
total_passed=0
total_failed=0

# Run R PostgreSQL tests
echo "========================================"
echo "Running R PostgreSQL Tests..."
echo "========================================"
docker exec valengr-rstudio Rscript /shared/tests/database/test_postgres_r.R
r_status=$?
echo ""

# Run Python PostgreSQL tests
echo "========================================"
echo "Running Python PostgreSQL Tests..."
echo "========================================"
docker exec valengr-python python /shared/tests/database/test_postgres_python.py
python_status=$?
echo ""

# Run Prolog PostgreSQL tests
echo "========================================"
echo "Running Prolog PostgreSQL Tests..."
echo "========================================"
docker exec valengr-prolog swipl -s /shared/tests/database/test_postgres_prolog.pl
prolog_status=$?
echo ""

# Overall summary
echo "========================================"
echo "Database Language Tests - Overall Summary"
echo "========================================"

if [ $r_status -eq 0 ]; then
    echo "✓ R PostgreSQL tests: PASSED"
    ((total_passed++))
else
    echo "✗ R PostgreSQL tests: FAILED"
    ((total_failed++))
fi

if [ $python_status -eq 0 ]; then
    echo "✓ Python PostgreSQL tests: PASSED"
    ((total_passed++))
else
    echo "✗ Python PostgreSQL tests: FAILED"
    ((total_failed++))
fi

if [ $prolog_status -eq 0 ]; then
    echo "✓ Prolog PostgreSQL tests: PASSED"
    ((total_passed++))
else
    echo "✗ Prolog PostgreSQL tests: FAILED"
    ((total_failed++))
fi

echo ""
echo "Test suites passed: $total_passed/3"
echo "Test suites failed: $total_failed/3"
echo "========================================"
echo ""
echo "Press any key to close..."
read -n 1

# Exit with failure if any tests failed
if [ $total_failed -gt 0 ]; then
    exit 1
fi
