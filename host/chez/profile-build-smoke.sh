#!/bin/sh

# profile-guided build smoke (RFC 0015 P2): `jolt build --profile-gen` makes a
# binary that writes Chez block counts as it exits, and `--profile-use` builds
# the same sources with them.
#
#   - the round trip: two runs, two dumps, one build that uses both, and its
#     binary prints what the plain one does
#   - a run with no JOLT_PROFILE_DATA writes NAME-PID.jpd in its directory
#   - profiled units and plain ones never share a cache entry: a --profile-use
#     build after a plain one compiles every app unit, and a plain build after
#     it compiles none
#   - a profile whose unit text changed fails the build and names the namespace,
#     instead of being silently ignored by Chez, and one from another build mode
#     fails saying so
#   - the flags refuse the builds they cannot do
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="${JOLT_BIN:-bin/jolt}"
case "$jolt" in /*) ;; *) jolt="$root/$jolt" ;; esac

# Same preflight as build-smoke.sh.
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  chez_bin="${JOLT_CHEZ:-$(command -v chez || command -v chezscheme || command -v scheme || command -v petite || true)}"
  if [ -n "$chez_bin" ]; then
    base="$(cd "$(dirname "$chez_bin")/.." 2>/dev/null && pwd)"
    for d in "$base"/lib/csv*/*/; do
      [ -f "${d}libkernel.a" ] && csv="${d%/}" && break
    done
  fi
fi
if ! command -v cc >/dev/null 2>&1 || [ -z "$csv" ] || [ ! -f "$csv/scheme.h" ] || [ ! -f "$csv/libkernel.a" ]; then
  echo "profile build smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
cp -R test/chez/repro-app "$tmp/app"
mkdir -p "$tmp/run"
# Caches of this gate's own: the instrumented runtime is a cache entry too, and
# the cache-isolation rows count compiles against a known starting state.
export JOLT_BUILD_CACHE_DIR="$tmp/build-cache"
export JOLT_RUNTIME_CACHE_DIR="$tmp/runtime-cache"
fails=0
pass=0
ok() { pass=$((pass + 1)); }
bad() { echo "  FAIL: $1"; fails=$((fails + 1)); }

# build NAME FLAGS... : the fixture into $tmp/NAME; the exit status is the build's
build() {
  name="$1"; shift
  JOLT_PWD="$tmp/app" JOLT_BUILD_PROFILE=1 "$jolt" build -m app.core -o "$tmp/$name" "$@" \
    >"$tmp/$name.log" 2>&1
}
# compiled NAME : "k/n" from the build's "compile app units (k/n compiled)" mark
compiled() { sed -n 's/.*compile app units (\([0-9]*\/[0-9]*\) compiled).*/\1/p' "$tmp/$1.log" | head -1; }

if ! build plain; then
  echo "  FAIL: the plain build exited non-zero"; tail -5 "$tmp/plain.log" | sed 's/^/    /'; exit 1
fi
want="$(cd "$tmp/run" && "$tmp/plain")"

# --- the round trip ------------------------------------------------------------
if ! build gen --profile-gen; then
  echo "  FAIL: --profile-gen build exited non-zero"; tail -5 "$tmp/gen.log" | sed 's/^/    /'; exit 1
fi
got1="$(cd "$tmp/run" && JOLT_PROFILE_DATA="$tmp/run1.jpd" "$tmp/gen" 2>"$tmp/gen1.err")"
got2="$(cd "$tmp/run" && JOLT_PROFILE_DATA="$tmp/run2.jpd" "$tmp/gen" 2>/dev/null)"
if [ "$got1" = "$want" ] && [ "$got2" = "$want" ]; then ok; else
  bad "the --profile-gen binary's output differs from the plain one's"; fi
if [ -s "$tmp/run1.jpd" ] && [ -s "$tmp/run2.jpd" ] && grep -q "wrote the block profile" "$tmp/gen1.err"; then ok; else
  bad "each --profile-gen run should write its JOLT_PROFILE_DATA file and say so"; sed 's/^/    /' "$tmp/gen1.err"; fi

# no JOLT_PROFILE_DATA: NAME-PID.jpd where it ran
(cd "$tmp/run" && "$tmp/gen" >/dev/null 2>&1)
if ls "$tmp/run"/gen-*.jpd >/dev/null 2>&1; then ok; else
  bad "a --profile-gen run with no JOLT_PROFILE_DATA should write gen-PID.jpd in its directory"; fi

if build use --profile-use "$tmp/run1.jpd" "$tmp/run2.jpd"; then
  got="$(cd "$tmp/run" && "$tmp/use")"
  if [ "$got" = "$want" ]; then ok; else bad "the --profile-use binary's output differs from the plain one's"; fi
else
  bad "the --profile-use build exited non-zero"; tail -5 "$tmp/use.log" | sed 's/^/    /'
fi

# --- cache isolation -----------------------------------------------------------
# plain filled the cache first, so a hit here would be a plain unit served to a
# profiled build; replain then has every unit in the cache already
n_use="$(compiled use)"
build replain
n_replain="$(compiled replain)"
case "$n_use" in
  */*) [ "${n_use%/*}" = "${n_use#*/}" ] && [ "${n_use%/*}" -gt 0 ] && ok \
         || bad "a --profile-use build should compile every app unit, not take plain ones from the cache (compiled $n_use)" ;;
  *) bad "no 'compile app units' mark in the --profile-use build's log" ;;
esac
case "$n_replain" in
  0/*) ok ;;
  *) bad "a plain build after a profiled one should compile nothing (compiled ${n_replain:-?})" ;;
esac

# --- a stale profile ------------------------------------------------------------
sed 's/42 :answer/43 :answer/' "$tmp/app/src/app/core.clj" > "$tmp/core.clj"
cp "$tmp/app/src/app/core.clj" "$tmp/core.clj.orig"
mv "$tmp/core.clj" "$tmp/app/src/app/core.clj"
if build stale --profile-use "$tmp/run1.jpd"; then
  bad "a --profile-use build over changed code should fail"
elif grep -q 'app\.core' "$tmp/stale.log" && grep -q -- '--profile-gen' "$tmp/stale.log"; then
  ok
else
  bad "the stale-profile failure should name app.core and say to run --profile-gen again"
  tail -5 "$tmp/stale.log" | sed 's/^/    /'
fi
mv "$tmp/core.clj.orig" "$tmp/app/src/app/core.clj"

# a profile from a release build used by an --opt one is a mode mismatch, said as one
if build optuse --opt --profile-use "$tmp/run1.jpd"; then
  bad "an --opt build should refuse a release build's profile"
elif grep -q 'is from a release build' "$tmp/optuse.log"; then ok; else
  bad "the mode-mismatch failure should name the profile's mode"; tail -3 "$tmp/optuse.log" | sed 's/^/    /'; fi

# --- the flags refuse what they cannot do ----------------------------------------
if build both --profile-gen --profile-use "$tmp/run1.jpd"; then
  bad "--profile-gen with --profile-use should be refused"
elif grep -q 'two builds' "$tmp/both.log"; then ok; else
  bad "--profile-gen with --profile-use: wrong message"; tail -3 "$tmp/both.log" | sed 's/^/    /'; fi
if build missing --profile-use "$tmp/nope.jpd"; then
  bad "--profile-use of a missing file should be refused"
elif grep -q 'no profile file' "$tmp/missing.log"; then ok; else
  bad "--profile-use of a missing file: wrong message"; tail -3 "$tmp/missing.log" | sed 's/^/    /'; fi
if build lib --profile-gen --library; then
  bad "--profile-gen --library should be refused"
elif grep -q 'not available with --library' "$tmp/lib.log"; then ok; else
  bad "--profile-gen --library: wrong message"; tail -3 "$tmp/lib.log" | sed 's/^/    /'; fi

echo "profile build smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
