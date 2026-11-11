#!/bin/sh
# provider-first-use: each case of test/chez/provider-first-use.clj in its own process,
# so the provider's first load is the one the case races
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
fails=0
for m in fiber future thread main-then-fiber fibers futures fibers-and-futures statics-on-fibers; do
  JOLT_AOT_CACHE=0 bin/jolt run test/chez/provider-first-use.clj "$m" 2>&1 | tail -1 | grep . || echo "provider-first-use $m: FAIL (no answer)"
done > "${TMPDIR:-/tmp}/provider-first-use.$$" 
cat "${TMPDIR:-/tmp}/provider-first-use.$$"
grep -q FAIL "${TMPDIR:-/tmp}/provider-first-use.$$" && fails=1
rm -f "${TMPDIR:-/tmp}/provider-first-use.$$"
if [ "$fails" -ne 0 ]; then echo "provider-first-use: failed"; exit 1; fi
echo "provider-first-use: passed"
