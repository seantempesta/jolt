#!/bin/sh

# reproducible build smoke (RFC 0015 P1): building the same sources twice gives
# every compiled unit identical text. Profile-guided builds depend on it: Chez
# matches a block profile to code by its source descriptor's CRC, so a unit
# whose text drifted between the --profile-gen build and the --profile-use one
# could not take the profile. The unit cache (build.ss bld-compile-app-units!)
# depends on it too: a unit that is not reproducible is a cache miss on every
# build.
#
# Each mode builds the fixture twice, the second time against the AOT namespace
# cache the first one filled, so a difference between compiling a namespace and
# loading it (a gensym counter, an iteration order) shows up as a diff. The
# build cache and the runtime cache are off: a cache hit copies the compiled
# unit, so its text on disk would not be the text this build emitted.
#
# The negative control edits one constant in one namespace and expects exactly
# that namespace's unit to change. It is what makes the identical comparisons
# mean something: a gate that compared nothing, or files no build writes, would
# pass them as well.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="${JOLT_BIN:-bin/jolt}"
case "$jolt" in /*) ;; *) jolt="$root/$jolt" ;; esac

# Same preflight as build-smoke.sh: a standalone build needs Chez's kernel dev
# files and a C compiler.
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
  echo "repro build smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
cp -R test/chez/repro-app "$tmp/app"
fails=0
pass=0

# build NAME AOT-DIR FLAGS... : the fixture into $tmp/NAME, its units in NAME.build
build() {
  name="$1"; aot="$2"; shift 2
  if ! JOLT_PWD="$tmp/app" JOLT_CACHE_DIR="$aot" JOLT_BUILD_CACHE=0 JOLT_RUNTIME_CACHE=0 \
       "$jolt" build -m app.core -o "$tmp/$name" "$@" >"$tmp/$name.log" 2>&1; then
    echo "  FAIL: jolt build $* ($name) exited non-zero"
    tail -5 "$tmp/$name.log" | sed 's/^/    /'
    exit 1
  fi
}

# units A B : the unit files A.build has, one per line, and the ones whose text
# differs in B.build (a unit missing from B counts as differing)
units() { (cd "$tmp/$1.build" && ls *.ss); }
differing() {
  for f in $(units "$1"); do
    cmp -s "$tmp/$1.build/$f" "$tmp/$2.build/$f" || echo "$f"
  done
}

for mode in default shaken; do
  case $mode in
    default) flags="" ;;
    shaken) flags="--tree-shake --opt" ;;
  esac
  # $flags is deliberately unquoted: it is a list of flags
  build "$mode-cold" "$tmp/aot-$mode" $flags
  build "$mode-warm" "$tmp/aot-$mode" $flags
  n="$(units "$mode-cold" | wc -l | tr -d ' ')"
  have_rt="$(units "$mode-cold" | grep -c '^runtime')"
  have_app="$(units "$mode-cold" | grep -c '^app-[0-9]')"
  # a shaken build is one app unit for the whole program, a plain one a unit
  # per namespace
  if [ "$n" -lt 4 ] || [ "$have_rt" -lt 1 ] || [ "$have_app" -lt 1 ]; then
    echo "  FAIL: $mode build wrote $n unit(s), expected the runtime, flat.ss, the launcher and the app's:"
    units "$mode-cold" | sed 's/^/    /'
    fails=$((fails + 1))
    continue
  fi
  d="$(differing "$mode-cold" "$mode-warm")"
  if [ -z "$d" ] && [ "$(units "$mode-cold")" = "$(units "$mode-warm")" ]; then
    pass=$((pass + 1))
  else
    echo "  FAIL: $mode build is not reproducible; units that differ between two builds:"
    printf '%s\n' "$d" | sed 's/^/    /'
    fails=$((fails + 1))
  fi
done

# Negative control: one edited constant in app.core changes app.core's unit and
# nothing else.
sed 's/42 :answer/43 :answer/' "$tmp/app/src/app/core.clj" > "$tmp/core.clj" \
  && mv "$tmp/core.clj" "$tmp/app/src/app/core.clj"
build edited "$tmp/aot-default"
d="$(differing default-cold edited)"
core_unit="$(cd "$tmp/default-cold.build" && grep -l 'set-chez-ns! "app.core"' app-[0-9]*.ss | head -1)"
if [ -n "$core_unit" ] && [ "$d" = "$core_unit" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: editing app.core should change its unit ($core_unit) and only it; changed:"
  printf '%s\n' "$d" | sed 's/^/    /'
  fails=$((fails + 1))
fi

echo "repro build smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
