#!/bin/sh
# build-empty-cache-entry: an empty file under a build-cache key is no compiled unit. Build
# test/chez/build-app with a cache of its own, empty every entry the build wrote (what two
# builds of one directory at once, one of them killed, left under a key), build again: the
# second binary runs and answers as the first. Before the entry was checked, the second
# build linked in units that define nothing, and its binary died at start on an unbound
# variable.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
jolt="${JOLT_BIN:-target/release/jolt}"
case "$jolt" in /*) ;; *) jolt="$root/$jolt" ;; esac
tmp="$(mktemp -d)"
build() { JOLT_PWD="$root/test/chez/build-app" JOLT_BUILD_CACHE_DIR="$tmp/cache" "$jolt" build -m app.core -o "$1" > "$1.log" 2>&1; }
build "$tmp/bin1" || { echo "build-empty-cache-entry: FAIL the first build exited non-zero"; sed -n '1,10p' "$tmp/bin1.log"; exit 1; }
want="$(cd / && "$tmp/bin1" alpha bb ccc 2>&1)"
n=0
for f in "$tmp/cache"/*.so "$tmp/cache"/*.vfasl; do [ -f "$f" ] && { : > "$f"; n=$((n + 1)); }; done
[ "$n" -gt 0 ] || { echo "build-empty-cache-entry: FAIL the first build wrote no cache entry to empty"; exit 1; }
build "$tmp/bin2" || { echo "build-empty-cache-entry: FAIL the build over $n empty entries exited non-zero"; sed -n '1,10p' "$tmp/bin2.log"; exit 1; }
got="$(cd / && "$tmp/bin2" alpha bb ccc 2>&1)"
if [ "$got" != "$want" ]; then
  echo "build-empty-cache-entry: FAIL the binary built over $n empty entries answers differently"
  echo "--- want ---"; echo "$want" | sed -n '1,6p'; echo "--- got ---"; echo "$got" | sed -n '1,6p'
  exit 1
fi
left=0
for f in "$tmp/cache"/*.so "$tmp/cache"/*.vfasl; do [ -f "$f" ] && [ ! -s "$f" ] && left=$((left + 1)); done
[ "$left" -eq 0 ] || { echo "build-empty-cache-entry: FAIL $left empty entries are still in the cache"; exit 1; }
echo "build-empty-cache-entry: passed ($n entries emptied, the second build compiled them again)"
