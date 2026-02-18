# Use Ubuntu as the base image
FROM ubuntu:22.04

# Install GnuCOBOL and build tools
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y gnucobol build-essential && \
    rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /workspace

# Copy the project files
COPY . .

# Default command: build and run the COBOL program
CMD ["/bin/bash"]
