FROM ubuntu:22.04
RUN sed -i 's|http://archive.ubuntu.com/ubuntu|http://mirrors.kernel.org/ubuntu|g' /etc/apt/sources.list && \
    sed -i 's|http://security.ubuntu.com/ubuntu|http://mirrors.kernel.org/ubuntu|g' /etc/apt/sources.list

# Add SWI-Prolog development PPA and install build dependencies
RUN apt-get update && \ 
    apt-get install -y software-properties-common && \
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
    git \
    build-essential \
    autotools-dev \
    autoconf \
    pkg-config \
    liblmdb-dev \
    curl \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Install Deno
RUN curl -fsSL https://deno.land/install.sh | sh && \
    mv /root/.deno/bin/deno /usr/local/bin/

# Build and install pl_lmdb
RUN cd /tmp && \
    git clone https://github.com/CodiePP/pl_lmdb.git && \
    cd pl_lmdb && \
    aclocal --force && autoheader --force && autoconf --force && \
    ./configure && \
    make swi && \
    mkdir -p /root/lib/sbcl && \
    cp src/lmdb.qlf /root/lib/sbcl/ && \
    cp pllmdb-* /root/lib/sbcl/pllmdb && \
    mkdir -p /root/.config/swi-prolog && \
    echo ":- assertz(file_search_path(sbcl,'/root/lib/sbcl'))." >> /root/.config/swi-prolog/init.pl && \
    ls -la /root/lib/sbcl/ && \
    cd / && rm -rf /tmp/pl_lmdb

# Verify LMDB installation
RUN swipl -q -g "use_module(sbcl(lmdb)), halt" || echo "LMDB module verification failed"

# Create working directory
WORKDIR /app

# Copy all F3 source files
COPY . /app/

# Build f3p binary
RUN deno compile --allow-read f3p.ts

# Create initialization file for LMDB path
RUN echo ":- assertz(file_search_path(sbcl,'/root/lib/sbcl'))." > lmdb_init.pl

# Build f3 binary with LMDB support (without foreign save for now)
RUN swipl --goal=main --stand_alone=true -o f3 -c lmdb_init.pl -c f3_assert.pl

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