#!/bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

echo "Deploying F3 Docker image..."

# Check if logged into Docker Hub
if ! docker info | grep -q "Username"; then
    echo -e "${RED}Not logged into Docker Hub. Please run 'docker login' first.${NC}"
    exit 1
fi

# Push the image
if docker push ivantsoninski/f3; then
    echo -e "${GREEN}Successfully deployed F3 to Docker Hub${NC}"
else
    echo -e "${RED}Failed to deploy F3${NC}"
    exit 1
fi
