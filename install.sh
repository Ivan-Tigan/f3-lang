#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "Installing F3..."

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo -e "${RED}Docker is not installed. Please install Docker first:${NC}"
    echo "https://docs.docker.com/get-docker/"
    exit 1
fi

# Create f3 script
cat > /tmp/f3 << 'EOF'
#!/bin/bash

DOCKER_IMAGE="ivantsoninski/f3"

# Check if Docker is running
if ! docker info >/dev/null 2>&1; then
    echo "Error: Docker is not running"
    exit 1
fi

# Handle dockerfile subcommand
if [[ "$1" == "dockerfile" ]]; then
    shift
    if [ -z "$1" ]; then
        echo "Usage: f3 dockerfile <entrypoint.f3>"
        exit 1
    fi
    ENTRYPOINT="$1"
    
    echo "FROM $DOCKER_IMAGE" > Dockerfile
    echo "COPY . /data" >> Dockerfile
    echo "WORKDIR /data" >> Dockerfile
    echo "CMD [\"f3\", \"run\", \"$ENTRYPOINT\"]" >> Dockerfile
    
    echo -e "${GREEN}Created Dockerfile with entrypoint: $ENTRYPOINT${NC}"
    exit 0
fi

# Extract port mapping if -p flag is present
PORT_MAP=""
if [[ "$1" == "-p" ]]; then
    PORT_MAP="-p $2"
    shift 2
fi

# Ensure image is pulled
if ! docker image inspect $DOCKER_IMAGE >/dev/null 2>&1; then
    echo "Pulling F3 image..."
    docker pull $DOCKER_IMAGE
fi

# Get current directory for volume mounting
CURRENT_DIR=$(pwd)

# Run the docker command with all arguments, adding --rm flag
docker run --rm -v "$CURRENT_DIR:/data" $PORT_MAP $DOCKER_IMAGE f3 "$@"
EOF

# Make script executable
chmod +x /tmp/f3

# Move to /usr/local/bin (requires sudo)
if sudo mv /tmp/f3 /usr/local/bin/f3; then
    echo -e "${GREEN}F3 installed successfully!${NC}"
    echo "You can now use f3 from anywhere."
    echo "Example: f3 -p 3000:3000 run myfile.f3"
    echo "New command: f3 dockerfile <entrypoint.f3>"
else
    echo -e "${RED}Failed to install F3${NC}"
    exit 1
fi

# Pull the Docker image
echo "Pulling F3 Docker image..."
if docker pull ivantsoninski/f3; then
    echo -e "${GREEN}F3 Docker image pulled successfully!${NC}"
else
    echo -e "${RED}Failed to pull F3 Docker image${NC}"
    exit 1
fi

echo -e "\n${GREEN}Installation complete!${NC}"