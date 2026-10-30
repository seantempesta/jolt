# The toolchain to build Jolt and run its gates on glibc Linux (arm64 on an Apple-silicon
# host, amd64 elsewhere: the image follows the Docker engine's architecture). Chez Scheme
# and Jolt are NOT in the image: ci/linux/run.sh mounts their sources and keeps what it
# builds in named volumes, so a source change rebuilds only what changed.
FROM debian:bookworm-slim
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential git ca-certificates file xxd perl rsync procps psmisc \
      liblz4-dev zlib1g-dev libncurses-dev uuid-dev \
    && rm -rf /var/lib/apt/lists/*
# Not root: root ignores file permissions, which several gates (io-file-perms, read-only
# directories) assert. The directories are made here so the named volumes mounted on them
# start owned by this user.
ENV LC_ALL=C.UTF-8
RUN useradd -m -u 1000 jolt && mkdir /opt/chez /work && chown jolt /opt/chez /work
USER jolt
WORKDIR /work
