#!/bin/sh
# fiber-second-mutator: each case of test/chez/fiber-second-mutator.clj in its own
# process, so each starts with one OS thread and no fiber spawned yet
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
fails=0
for m in lazy-alloc lazy-twice iterator-twice chm-compute; do
  JOLT_AOT_CACHE=0 bin/jolt run test/chez/fiber-second-mutator.clj "$m" 2>&1 | tail -1 | grep "^fiber-second-mutator $m: " || echo "fiber-second-mutator $m: FAIL (no answer)"
done > "${TMPDIR:-/tmp}/fiber-second-mutator.$$"
cat "${TMPDIR:-/tmp}/fiber-second-mutator.$$"
grep -q FAIL "${TMPDIR:-/tmp}/fiber-second-mutator.$$" && fails=1
rm -f "${TMPDIR:-/tmp}/fiber-second-mutator.$$"
if [ "$fails" -ne 0 ]; then echo "fiber-second-mutator: failed"; exit 1; fi
echo "fiber-second-mutator: passed"
