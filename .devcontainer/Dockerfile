FROM ubuntu:22.04

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y gnucobol build-essential && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

# [Optional] Uncomment to install additional OS packages
# RUN apt-get install -y <your-package-list>

# [Optional] Copy project files if you want to pre-build in the container
# COPY . .

# [Optional] Pre-build the COBOL program
# RUN cobc -x src/InCollege.cob -o InCollege

# Default command (override in devcontainer.json if needed)
CMD ["/bin/bash"]
