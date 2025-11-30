#!/bin/bash

# LeanCMS Build Script
set -e

echo "🔨 Building LeanCMS Docker image..."

# Get version from environment or default to latest
VERSION=${VERSION:-latest}
IMAGE_NAME="leancms"

# Build the image
echo "Building ${IMAGE_NAME}:${VERSION}..."
docker build -t ${IMAGE_NAME}:${VERSION} .
docker tag ${IMAGE_NAME}:${VERSION} ivantsoninski/${IMAGE_NAME}:${VERSION}

# Also tag as latest if we're building a specific version
if [ "$VERSION" != "latest" ]; then
    echo "Tagging as latest..."
    docker tag ${IMAGE_NAME}:${VERSION} ${IMAGE_NAME}:latest
    docker tag ${IMAGE_NAME}:${VERSION} ivantsoninski/${IMAGE_NAME}:latest
fi

echo "✅ Build complete!"
echo "Built images:"
docker images | grep ${IMAGE_NAME}