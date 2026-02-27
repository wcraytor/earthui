#!/bin/bash
# Create Python Virtual Environment for ValEngr
#
# Usage:
#   ./create_python_virtual_environment.sh
#
# This script creates a Python virtual environment and installs
# all required dependencies for the ValEngr project.

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

echo "Creating Python virtual environment for ValEngr..."
echo "Directory: $SCRIPT_DIR"
echo ""

# Remove existing venv if present
if [ -d "venv" ]; then
    echo "Removing existing venv..."
    rm -rf venv
fi

# Create new virtual environment
echo "Creating virtual environment..."
python3 -m venv venv

# Verify it was created
if [ ! -f "venv/bin/python" ]; then
    echo "ERROR: Failed to create virtual environment"
    exit 1
fi

# Activate and install dependencies
echo "Installing dependencies..."
source venv/bin/activate

# Upgrade pip first
pip install --upgrade pip

# Install required packages from requirements.txt
if [ -f "requirements.txt" ]; then
    pip install -r requirements.txt
else
    echo "WARNING: requirements.txt not found, installing basic packages..."
    pip install requests psycopg2-binary paramiko python-dotenv
fi

echo ""
echo "Virtual environment created successfully!"
echo ""
echo "Python version: $(python --version)"
echo "Pip version: $(pip --version)"
echo ""
echo "Installed packages:"
pip list
echo ""
echo "To activate manually, run:"
echo "  source venv/bin/activate"
