#!/bin/sh
# Runs inside the container (ci/linux/run.sh): Chez from /chez-src into /opt/chez (a
# volume, stamped with the Chez commit), Jolt from /src into /work (a volume), then the
# gates or the command asked for.
set -eu
jobs=${JOLT_LINUX_JOBS:-4}
if [ "$(cat /opt/chez/.stamp 2>/dev/null)" != "$CHEZ_ID" ]; then
  echo "== chez $CHEZ_ID: configure + make -j$jobs"
  rm -rf /tmp/chez-build /opt/chez/*
  cp -a /chez-src /tmp/chez-build
  cd /tmp/chez-build
  # --threads: Jolt requires it; -fPIC: the kernel is folded into a shared object (as bin/build chez)
  ./configure --threads --disable-x11 --disable-curses --installprefix=/opt/chez CFLAGS+=-fPIC >/tmp/chez-configure.log 2>&1 || { tail -30 /tmp/chez-configure.log; exit 1; }
  make -j"$jobs" >/tmp/chez-make.log 2>&1 || { tail -30 /tmp/chez-make.log; exit 1; }
  make install >/tmp/chez-install.log 2>&1 || { tail -30 /tmp/chez-install.log; exit 1; }
  echo "$CHEZ_ID" > /opt/chez/.stamp
  cd /; rm -rf /tmp/chez-build
fi
# the Jolt tree, without the Mac's outputs; /work/target and /work/.jolt stay (the build cache)
rsync -rlptD --delete --exclude .git --exclude /target --exclude /build --exclude .jolt --exclude /.cache/local /src/ /work/
cd /work
git config --global --add safe.directory "*"
export JOLT_VERSION=${JOLT_VERSION:-linux-gates}
export JOLT_CHEZ=/opt/chez/bin/scheme
export HOME=/work/.home; mkdir -p "$HOME"
if [ "$1" = -- ]; then shift; exec "$@"; fi
rc=0
for gate in "$@"; do
  echo "== gate $gate"
  if make -o submodules -o deps CHEZ=/opt/chez/bin/scheme "$gate"; then echo "== GATE $gate: PASS"
  else echo "== GATE $gate: FAIL"; rc=1; [ -n "$KEEP_GOING" ] || exit 1; fi
done
exit $rc
