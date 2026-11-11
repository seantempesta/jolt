#!/bin/sh
# fiber-uncaught: an uncaught throw of a go block or a fiber body reaches the default
# uncaught-exception handler (test/chez/fiber-uncaught.clj), and nothing of it goes to stderr
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
out="${TMPDIR:-/tmp}/fiber-uncaught.$$"
JOLT_AOT_CACHE=0 bin/jolt run test/chez/fiber-uncaught.clj > "$out" 2> "$out.err"
tail -1 "$out"
fails=0
tail -1 "$out" | grep -q "^fiber-uncaught: ok$" || fails=1
if grep -q "^Exception in " "$out.err"; then echo "fiber-uncaught: stderr still got: $(grep -m1 '^Exception in ' "$out.err")"; fails=1; fi
rm -f "$out" "$out.err"
if [ "$fails" -ne 0 ]; then echo "fiber-uncaught: failed"; exit 1; fi
echo "fiber-uncaught: passed"
