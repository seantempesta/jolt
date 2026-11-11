#!/bin/sh
# fiber-unread: a spawned fiber whose error nobody read is reported once when it is
# collected (test/chez/fiber-unread.clj); each case in its own process. The stderr
# case sets no handler, so its one report must be on stderr, in the shape the other
# uncaught throws use.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
tmp="${TMPDIR:-/tmp}/fiber-unread.$$"
fails=0
for m in unread joined-later monitored value many stderr; do
  JOLT_AOT_CACHE=0 bin/jolt run test/chez/fiber-unread.clj "$m" > "$tmp.out" 2> "$tmp.err"
  tail -1 "$tmp.out" | grep "^fiber-unread $m: " || echo "fiber-unread $m: FAIL (no answer)"
  if [ "$m" = stderr ]; then
    n=$(grep -c "^Exception in fiber whose error was never read:" "$tmp.err")
    k=$(grep -c "to stderr 0" "$tmp.err")
    if [ "$n" -ne 1 ] || [ "$k" -lt 1 ]; then echo "fiber-unread stderr: FAIL ($n reports on stderr): $(head -3 "$tmp.err")"; fi
  elif grep -q "^Exception in " "$tmp.err"; then
    echo "fiber-unread $m: FAIL stderr got: $(grep -m1 '^Exception in ' "$tmp.err")"
  fi
done > "$tmp"
cat "$tmp"
grep -q FAIL "$tmp" && fails=1
rm -f "$tmp" "$tmp.out" "$tmp.err"
if [ "$fails" -ne 0 ]; then echo "fiber-unread: failed"; exit 1; fi
echo "fiber-unread: passed"
