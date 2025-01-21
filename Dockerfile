FROM ubuntu:22.04

# Add SWI-Prolog development PPA
RUN apt-get update && apt-get install -y software-properties-common && \
    add-apt-repository ppa:swi-prolog/devel && \
    apt-get update && \
    apt-get install -y \
    swi-prolog \
    libc6 \
    libgcc-s1 \
    libpthread-stubs0-dev \
    libtinfo6 \
    libgmp10 \
    zlib1g \
    && rm -rf /var/lib/apt/lists/*

# Create working directory
WORKDIR /app

# Copy the executables
COPY f3 f3p /app/

# Make executables executable
RUN chmod +x /app/f3 /app/f3p

# Create a directory for user files
RUN mkdir /data

# Set environment variables
ENV PATH="/app:${PATH}"
ENV F3_HOME=/app

# Set working directory to /data where users will mount their files
WORKDIR /data

# Command template (users can override)
CMD ["f3", "run", "todo.f3"]