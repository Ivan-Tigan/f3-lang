#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "Building F3 (Docker-only build)..."

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo -e "${RED}Docker is not installed. Please install it first.${NC}"
    exit 1
fi

# Build Docker image with everything inside
echo "Building Docker image..."
if docker build -t ivantsoninski/f3 .; then
    echo -e "${GREEN}Docker image built successfully${NC}"
else
    echo -e "${RED}Failed to build Docker image${NC}"
    exit 1
fi

echo -e "\n${GREEN}Docker-only build complete!${NC}"
echo "Use: docker run -v \$(pwd):/data ivantsoninski/f3 f3 run your_program.f3"
