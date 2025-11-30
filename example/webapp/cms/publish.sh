#!/bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

echo "Publishing LeanCMS Docker image..."

# Check if logged into Docker Hub
if ! docker info | grep -q "Username"; then
    echo -e "${RED}Not logged into Docker Hub. Please run 'docker login' first.${NC}"
    exit 1
fi

# Push the image
if docker push ivantsoninski/leancms; then
    echo -e "${GREEN}Successfully published LeanCMS to Docker Hub${NC}"
else
    echo -e "${RED}Failed to publish LeanCMS${NC}"
    exit 1
fi