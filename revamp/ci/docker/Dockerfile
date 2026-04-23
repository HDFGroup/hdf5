# ci/docker/Dockerfile
# =====================
# Container definition for the Revamp pipeline runner.
# Includes all static analysis tools + Python dependencies.
#
# Owner: Abhinav (Pipeline & CI Lead)
#
# Usage:
#   docker build -t revamp-pipeline -f ci/docker/Dockerfile .
#   docker run --rm -v $(pwd)/hdf5-sandbox:/hdf5 revamp-pipeline

FROM ubuntu:24.04

# Prevent interactive prompts during apt installs
ENV DEBIAN_FRONTEND=noninteractive

# System dependencies
RUN apt-get update && apt-get install -y \
    python3 python3-pip \
    cmake ninja-build \
    gcc g++ \
    zlib1g-dev \
    cppcheck \
    clang clang-tidy \
    git \
    && rm -rf /var/lib/apt/lists/*

# Python pipeline dependencies
WORKDIR /revamp
COPY requirements.txt .
RUN pip3 install --break-system-packages -r requirements.txt

# Pipeline source
COPY . .

# Default: run the orchestrator
CMD ["python3", "orchestrator.py"]
