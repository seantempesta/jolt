#!/bin/sh
# pgo-bench.sh — what a profile-guided build buys (RFC 0015 P3).
#
#   ci/pgo-bench.sh <jolt> [max-ratio] [workload...]
#
# For each workload and each build mode (the default release build, and
# --direct-link --opt), one jolt builds the program three ways: plain, with
# --profile-gen, and with --profile-use fed by one run of the --profile-gen
# binary on the input that is then timed. The plain and profiled binaries are
# timed alternately, after a discarded warm-up, and the row is the min of
# $PGO_ROUNDS runs per side (noise only ever adds time), as ci/bench-gate.sh
# does. The ratio is profiled/plain: below 1 is a speedup.
#
# Training on the timed input is the best case for a profile. It is what P3
# asks first (does block PGO help at all); a profile from a different input
# can only do less.
#
# Workloads are the bench/run.sh suite plus three programs larger than any
# bench: the ray tracer's two variants (jolt-lang/examples, found through
# $PGO_EXAMPLES, a checkout of it) and honey.sql's format loop
# (ci/pgo-workloads/honeysql, a git dep, so it needs the network once). A
# workload whose checkout is missing is skipped, unless PGO_REQUIRE_ALL=1.
#
# The build columns are seconds for the plain, --profile-gen and --profile-use
# builds, against warm caches for the runtime: a --profile-use build compiles
# the runtime and clojure.core again with the profile, every time, since each
# workload's profile is its own.
#
#   PGO_ROUNDS=3      timed runs per side
#   PGO_SHARD=i/n     only every n-th workload, starting at the i-th
#   PGO_MODES="release opt"
#   PGO_OUT=file      also append the rows there (the CI summary reads it)
#
# Exits non-zero when a profiled binary is more than max-ratio (default 1.10)
# times slower than the plain one, or when a workload fails to build or run.
set -eu

jolt="${1:-}"; max="${2:-1.10}"
[ "$#" -gt 2 ] && { shift 2; only="$*"; } || only=""
[ -n "$jolt" ] || { echo "usage: $0 <jolt> [max-ratio] [workload...]" >&2; exit 2; }
case "$jolt" in /*) ;; *) jolt="$(pwd)/$jolt" ;; esac
[ -x "$jolt" ] || { echo "not executable: $jolt" >&2; exit 2; }

root="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
rounds="${PGO_ROUNDS:-3}"
modes="${PGO_MODES:-release opt}"
shard="${PGO_SHARD:-1/1}"
shard_i="${shard%/*}"; shard_n="${shard#*/}"
examples="${PGO_EXAMPLES:-}"
out="${PGO_OUT:-}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Same preflight as bench/run.sh: every build here links a standalone binary.
csv="${JOLT_CHEZ_CSV:-}"
if [ -z "$csv" ]; then
  chez_bin="${JOLT_CHEZ:-$(command -v chez || command -v scheme || command -v petite || true)}"
  if [ -n "$chez_bin" ]; then
    base="$(cd "$(dirname "$chez_bin")/.." 2>/dev/null && pwd)"
    for d in "$base"/lib/csv*/*/; do
      [ -f "${d}libkernel.a" ] && csv="${d%/}" && break
    done
  fi
fi
if [ -z "$csv" ] || [ ! -f "$csv/libkernel.a" ] || [ ! -f "$csv/scheme.h" ] || ! command -v cc >/dev/null 2>&1; then
  echo "pgo bench: needs Chez kernel dev files (libkernel.a + scheme.h) and cc; set JOLT_CHEZ_CSV" >&2
  exit 2
fi
export JOLT_CHEZ_CSV="$csv"

# name dir ns arg, one per line
{
  for spec in $(sh "$root/bench/run.sh" --list); do
    echo "${spec%%:*} $root/bench ${spec%%:*} ${spec##*:}"
  done
  ray=-; [ -n "$examples" ] && ray="$examples/ray-tracer"
  echo "ray-baseline $ray ray-baseline 3"
  echo "ray-typed $ray ray-typed 3"
  echo "honeysql $root/ci/pgo-workloads/honeysql pgo-honeysql 5000"
} > "$work/workloads"

fails=0; rows=0; skipped=0; k=0
hdr="$(printf '%-18s %-7s %10s %10s %7s %6s %6s %6s' workload mode plain-ms pgo-ms ratio b-pl b-gen b-use)"
echo "pgo bench: $("$jolt" --version 2>/dev/null || echo '?'), min of $rounds runs per side, fail above ${max}x"
echo "$hdr"
[ -n "$out" ] && echo "$hdr" >> "$out"
: > "$work/rows"

now() { perl -MTime::HiRes=time -e 'printf "%.1f", time()'; }
# build <dir> <ns> <out> <flags...> ; prints the seconds it took
build() {
  d="$1"; ns="$2"; o="$3"; shift 3
  t0="$(now)"
  ( cd "$d" && JOLT_PWD="$PWD" "$jolt" build -m "$ns" -o "$o" "$@" ) >"$o.log" 2>&1 || return 1
  awk "BEGIN{printf \"%.0f\", $(now) - $t0}"
}
# timed <dir> <binary> <arg> ; run where bench/run.sh runs it, print its mean
timed() { ( cd "$1" && "$2" "$3" 2>/dev/null ) | awk '/^mean:/{print $2}'; }
# what a run prints apart from its timings, to compare plain with profiled
answer() { ( cd "$1" && "$2" "$3" 2>&1 ) | grep -v '^runs:' | grep -v '^mean:' | grep -v 'ms' || true; }
minof() { awk "BEGIN{m=\"$1\"+0; v=\"$2\"+0; if (v>0 && (m==0 || v<m)) m=v; print m}"; }
fail() { echo "  FAIL $1"; fails=$((fails + 1)); }

while read -r name dir ns arg; do
  k=$((k + 1))
  [ $(( (k - shard_i) % shard_n )) -eq 0 ] && [ "$k" -ge "$shard_i" ] || continue
  if [ -n "$only" ]; then
    want=0; for o in $only; do [ "$o" = "$name" ] && want=1; done
    [ "$want" = 1 ] || continue
  fi
  if [ "$dir" = - ] || [ ! -d "$dir" ]; then
    if [ "${PGO_REQUIRE_ALL:-}" = 1 ]; then fail "$name: no checkout (set PGO_EXAMPLES)"
    else echo "  SKIP $name: no checkout (set PGO_EXAMPLES)"; skipped=$((skipped + 1)); fi
    continue
  fi
  for mode in $modes; do
    case "$mode" in
      release) flags="" ;;
      opt) flags="--direct-link --opt" ;;
      *) echo "unknown mode: $mode" >&2; exit 2 ;;
    esac
    w="$work/$name-$mode"
    # $flags is deliberately unquoted: it is a list of flags
    if ! bpl="$(build "$dir" "$ns" "$w.plain" $flags)"; then
      fail "$name $mode: the plain build failed"; sed -n '1,15p' "$w.plain.log"; continue; fi
    if ! bgen="$(build "$dir" "$ns" "$w.gen" $flags --profile-gen)"; then
      fail "$name $mode: the --profile-gen build failed"; sed -n '1,15p' "$w.gen.log"; continue; fi
    ( cd "$dir" && JOLT_PROFILE_DATA="$w.jpd" "$w.gen" "$arg" ) >"$w.train.log" 2>&1 || true
    if [ ! -s "$w.jpd" ]; then
      fail "$name $mode: the --profile-gen run wrote no profile"; tail -5 "$w.train.log"; continue; fi
    if ! buse="$(build "$dir" "$ns" "$w.use" $flags --profile-use "$w.jpd")"; then
      fail "$name $mode: the --profile-use build failed"; sed -n '1,15p' "$w.use.log"; continue; fi
    # the warm-up runs, discarded as timings: the profiled binary must print
    # every line the plain one prints the same way twice. A line that moves
    # between two plain runs (gc-arrays' collection counts) is a measurement,
    # not an answer, so it is not compared.
    answer "$dir" "$w.plain" "$arg" > "$w.plain.out"
    answer "$dir" "$w.plain" "$arg" > "$w.plain2.out"
    answer "$dir" "$w.use" "$arg" > "$w.use.out"
    grep -Fxf "$w.plain2.out" "$w.plain.out" > "$w.stable.out" || true
    if grep -Fxvf "$w.use.out" "$w.stable.out" > "$w.missing.out"; then
      fail "$name $mode: the profiled binary does not print what the plain one does"
      sed -n '1,10p' "$w.missing.out" | sed 's/^/    missing: /'; continue; fi
    pmin=""; umin=""; i=0
    while [ "$i" -lt "$rounds" ]; do
      i=$((i + 1))
      pmin="$(minof "$pmin" "$(timed "$dir" "$w.plain" "$arg")")"
      umin="$(minof "$umin" "$(timed "$dir" "$w.use" "$arg")")"
    done
    if [ "$(awk "BEGIN{print (\"$pmin\"+0 <= 0 || \"$umin\"+0 <= 0)}")" = 1 ]; then
      fail "$name $mode: no timing (the program printed no mean:)"; continue; fi
    ratio="$(awk "BEGIN{printf \"%.2f\", $umin/$pmin}")"
    over="$(awk "BEGIN{print ($ratio > $max)}")"
    rows=$((rows + 1))
    row="$(printf '%-18s %-7s %10.1f %10.1f %6sx %6s %6s %6s' "$name" "$mode" "$pmin" "$umin" "$ratio" "$bpl" "$bgen" "$buse")"
    [ "$over" = 1 ] && row="$row  <-- SLOWER"
    echo "$row"
    echo "$row" >> "$work/rows"
    [ -n "$out" ] && echo "$row" >> "$out"
    [ "$over" = 1 ] && fails=$((fails + 1))
    rm -f "$w".plain "$w".gen "$w".use
  done
done < "$work/workloads"

echo
if [ "$rows" -eq 0 ] && [ "$fails" -eq 0 ]; then
  # named workloads land in one shard; the others have nothing to do
  if [ -n "$only" ] && [ "$shard_n" -gt 1 ]; then
    echo "pgo bench: none of the named workloads is in shard $shard"; exit 0
  fi
  echo "pgo bench: FAILED — nothing was measured"; exit 1
fi
for mode in $modes; do
  awk -v m="$mode" '$2 == m { r = $5; sub(/x$/, "", r); s += log(r); n++ }
       END { if (n) printf "pgo bench: %s geomean ratio %.3f over %d workloads\n", m, exp(s / n), n }' \
    "$work/rows"
done
skips=""; [ "$skipped" -gt 0 ] && skips=", $skipped skipped"
if [ "$fails" -gt 0 ]; then
  echo "pgo bench: FAILED — $fails problem(s) over $rows row(s)$skips"
  exit 1
fi
echo "pgo bench: passed — $rows row(s) within ${max}x$skips"
