#!/bin/sh
# aot-cache budget: the AOT cache is kept under JOLT_CACHE_MB, oldest written
# first (loader.ss aot-prune-to-budget!), and a hit writes nothing.
#
# Three namespaces are published and aged an hour, 40 and 20 minutes (their
# files' mtimes set back), the oldest is then hit, and a fourth is published
# under a budget that holds about three and a half of them: the one evicted is
# the oldest WRITTEN (a), whose hit left its mtime where it was.
# Every namespace still loads after, the evicted one by recompiling; an
# artifact deleted behind the loader's back recompiles; a young artifact
# survives a budget of nothing; and nothing outside the cache directory is
# touched: a sibling directory and a generation that is a symbolic link to one,
# each holding old .so files, keep them.

set -e
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="bin/jolt"
work="$(mktemp -d "${TMPDIR:-/tmp}/aot-budget.XXXXXX")"
trap 'rm -rf "$work"' EXIT
cache="$work/cache"
outside="$work/outside"
mkdir -p "$cache" "$outside" "$work/src/mylib"
fails=0
pass() { echo "PASS: $1"; }
fail() { echo "FAIL: $1"; fails=$((fails+1)); }

for n in a b c d; do
  cat > "$work/src/mylib/$n.clj" <<CLJ
(ns mylib.$n)
(defn answer [] (str "$n" (reduce + (range 10))))
CLJ
done

# require the namespaces named, print their answers on one line
run() {
  budget="$1"; shift
  forms=""
  for n in "$@"; do forms="$forms (require 'mylib.$n) (print (mylib.$n/answer))"; done
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_CACHE_MB="$budget" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps)
    (jolt.deps/add-deps {:deps {'mylib/mylib {:local/root \"$work\"}}})
    $forms (println)" 2>"$work/err" | tail -1
}
art() { find "$cache" -name "mylib.$1-*.$2" -type f | head -1; }
age() { perl -e '$t = time - shift; utime($t, $t, @ARGV) or die "utime"' "$@"; }
bytes() { perl -e '$s = 0; $s += -s $_ for @ARGV; print $s' "$@"; }

# what must never be touched: an old .so beside the cache, and a "generation"
# that is a link to a directory outside it
mkdir -p "$outside/v1"
echo keep > "$outside/v1/mylib.z-1-2-3.so"; echo keep > "$outside/v1/mylib.z-1-2-3.scm"
echo keep > "$work/beside.so"
ln -s "$outside" "$cache/linked-generation"
age 7200 "$outside/v1/mylib.z-1-2-3.so" "$outside/v1/mylib.z-1-2-3.scm" "$work/beside.so"
outside_before="$(find "$outside" "$work/beside.so" -type f | wc -l | tr -d ' ')"

big=100000   # MB: nothing is evicted
out="$(run $big a b c)"
[ "$out" = "a45b45c45" ] && pass "three published: $out" || { fail "first run answered '$out'"; cat "$work/err"; }
for n in a b c; do
  for x in so scm; do [ -n "$(art $n $x)" ] || fail "no .$x for mylib.$n"; done
done
age 3600 "$(art a so)" "$(art a scm)"
age 2400 "$(art b so)" "$(art b scm)"
age 1200 "$(art c so)" "$(art c scm)"
one="$(bytes "$(art b so)" "$(art b scm)")"
three="$(bytes "$(art a so)" "$(art a scm)" "$(art b so)" "$(art b scm)" "$(art c so)" "$(art c scm)")"

# a hit on the oldest writes nothing: its .so keeps the mtime it was aged to
a_before="$(perl -e 'print +(stat shift)[9]' "$(art a so)")"
out="$(run $big a)"
a_after="$(perl -e 'print +(stat shift)[9]' "$(art a so)")"
[ "$out" = "a45" ] && [ "$a_after" = "$a_before" ] && pass "a hit writes nothing to its artifact" \
  || fail "the hit answered '$out', its .so mtime moved $a_before -> $a_after"

# the fourth publishes under a budget of three and a half artifacts
budget="$(perl -e 'printf "%.6f", ($ARGV[0] + $ARGV[1] / 2) / 1048576' "$three" "$one")"
out="$(run "$budget" d)"
[ "$out" = "d45" ] || fail "the fourth answered '$out'"
if [ -z "$(art a so)" ] && [ -z "$(art a scm)" ]; then pass "the oldest written (a) was evicted, .so and .scm, its hit notwithstanding"
else fail "a was not evicted: $(art a so) $(art a scm)"; fi
for n in b c d; do
  [ -n "$(art $n so)" ] && [ -n "$(art $n scm)" ] && pass "$n kept" || fail "$n was evicted"
done
[ -n "$(find "$cache" -name 'mylib.a-*' ! -name '*.so' ! -name '*.scm' -type f | head -1)" ] \
  && pass "a's key sidecars stay" || fail "a's key sidecars were deleted"

# every one still loads; the evicted one recompiles and is published again
out="$(run $big a b c d)"
[ "$out" = "a45b45c45d45" ] && [ -n "$(art a so)" ] && pass "all four load after the prune: $out" \
  || { fail "after the prune: '$out'"; cat "$work/err"; }

# an artifact deleted behind the loader's back is a miss that recompiles
rm -f "$(art c so)"
out="$(run $big c)"
[ "$out" = "c45" ] && [ -n "$(art c so)" ] && pass "a .so deleted under the cache recompiles" \
  || { fail "a deleted .so answered '$out'"; cat "$work/err"; }
rm -f "$(art c so)" "$(art c scm)"
out="$(run $big c)"
[ "$out" = "c45" ] && pass "a whole artifact deleted recompiles" || { fail "a deleted artifact answered '$out'"; cat "$work/err"; }

# a budget of nothing evicts only what is past the grace period
age 3600 "$(art a so)" "$(art a scm)" "$(art b so)" "$(art b scm)"
rm -f "$(art d so)" "$(art d scm)"
out="$(run 0.000001 d)"
if [ "$out" = "d45" ] && [ -z "$(art a so)" ] && [ -z "$(art b so)" ] && [ -n "$(art c so)" ] && [ -n "$(art d so)" ]; then
  pass "under a budget of nothing the old go and the young (c, d) stay"
else fail "budget of nothing: '$out' a=$(art a so) b=$(art b so) c=$(art c so) d=$(art d so)"; fi

outside_after="$(find "$outside" "$work/beside.so" -type f | wc -l | tr -d ' ')"
[ "$outside_before" = "$outside_after" ] && [ -L "$cache/linked-generation" ] \
  && pass "nothing outside the cache was touched ($outside_after files)" \
  || fail "outside the cache: $outside_before files before, $outside_after after"

if [ "$fails" -gt 0 ]; then echo "aot-cache-budget: $fails failed"; exit 1; fi
echo "aot-cache-budget: passed"
