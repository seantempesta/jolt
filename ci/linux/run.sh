#!/bin/sh
# Build Jolt from source and run its gates in a Linux container (Docker).
#
#   ci/linux/run.sh [--chez DIR] [--keep-going] GATE...      e.g.  ci/linux/run.sh unit fibers
#   ci/linux/run.sh [--chez DIR] -- CMD...                    any command in the prepared tree
#
# GATE is a Makefile target (make -o submodules -o deps GATE). The Jolt checkout (this
# repository, read-only mount) is copied to the volume jolt-linux-work, which also holds
# every build output (target/), so the Mac tree's target/ is never written. Chez Scheme is
# built from DIR (a checkout of Chez, --chez or $JOLT_CHEZ_SRC; Seon's reference-code/chez,
# whose patches the fibers gates rely on) into the volume jolt-linux-chez, once per Chez
# commit. Vendored submodules (vendor/) must be checked out in this tree.
# Env: JOLT_LINUX_JOBS (make -j, default 4), JOLT_LINUX_CPUS (container cpus, default 4),
# JOLT_LINUX_PLATFORM (e.g. linux/amd64 to force an architecture).
set -eu
here=$(cd "$(dirname "$0")" && pwd)
src=$(cd "$here/../.." && pwd)
chez=${JOLT_CHEZ_SRC:-}
keep=
while [ $# -gt 0 ]; do
  case "$1" in
    --chez) chez=$2; shift 2 ;;
    --keep-going) keep=1; shift ;;
    *) break ;;
  esac
done
[ -n "$chez" ] && [ -f "$chez/c/thread.c" ] || { echo "run.sh: --chez DIR (or JOLT_CHEZ_SRC) must be a Chez Scheme checkout" >&2; exit 2; }
chez=$(cd "$chez" && pwd)
[ $# -gt 0 ] || { echo "usage: ci/linux/run.sh [--chez DIR] [--keep-going] GATE... | -- CMD..." >&2; exit 2; }
plat=
[ -z "${JOLT_LINUX_PLATFORM:-}" ] || plat="--platform $JOLT_LINUX_PLATFORM"
# shellcheck disable=SC2086
docker build -q $plat -t jolt-linux:bookworm "$here" >/dev/null
chezid=$(git -C "$chez" rev-parse HEAD)
# shellcheck disable=SC2086
exec docker run --rm $plat --cpus "${JOLT_LINUX_CPUS:-4}" \
  -v "$src":/src:ro -v "$chez":/chez-src:ro \
  -v jolt-linux-chez:/opt/chez -v jolt-linux-work:/work \
  -e CHEZ_ID="$chezid" -e JOLT_LINUX_JOBS="${JOLT_LINUX_JOBS:-4}" -e KEEP_GOING="$keep" \
  jolt-linux:bookworm sh /src/ci/linux/inside.sh "$@"
