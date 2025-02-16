#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "Building F3..."

# Check if required tools are installed
if ! command -v swipl &> /dev/null; then
    echo -e "${RED}SWI-Prolog is not installed. Please install it first.${NC}"
    exit 1
fi

if ! command -v deno &> /dev/null; then
    echo -e "${RED}Deno is not installed. Please install it first.${NC}"
    exit 1
fi

# Build f3 binary
echo "Building f3 binary..."
if swipl --goal=main --stand_alone=true --foreign=save -o f3 -c f3_assert.pl; then
    echo -e "${GREEN}f3 binary built successfully${NC}"
else
    echo -e "${RED}Failed to build f3 binary${NC}"
    exit 1
fi

# Build f3p binary
echo "Building f3p binary..."
if deno compile --allow-read f3p.ts; then
    echo -e "${GREEN}f3p binary built successfully${NC}"
else
    echo -e "${RED}Failed to build f3p binary${NC}"
    exit 1
fi

# Build Docker image
echo "Building Docker image..."
if docker build -t ivantsoninski/f3 .; then
    echo -e "${GREEN}Docker image built successfully${NC}"
else
    echo -e "${RED}Failed to build Docker image${NC}"
    exit 1
fi

echo -e "\n${GREEN}Build complete!${NC}"
