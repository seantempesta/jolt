#!/bin/sh
# stack-limit-smoke.sh — JOLT_STACK_LIMIT and the default it overrides, through a
# built jolt (test/stack_limit_test.clj has the per-mode programs). The limit is
# read once at startup, so every setting is its own process.
#
#   sh test/chez/stack-limit-smoke.sh [jolt-binary]
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
J="${1:-target/release/jolt}"
T=test/stack_limit_test.clj
CAP=120
pass=0; fail=0

# run MODE [VAR=VALUE...] -> out, rc (142 = killed at CAP seconds)
run() {
  mode="$1"; shift
  out=$(env JOLT_NO_USER_DEPS=1 "$@" perl -e 'alarm shift; exec @ARGV' "$CAP" "$J" run "$T" "$mode" < /dev/null 2>&1)
  rc=$?
}
ok() { pass=$((pass+1)); }
bad() { fail=$((fail+1)); echo "FAIL stacklimit: $1 — rc=$rc out=[$out]"; }
# expect LABEL WANT-LINE-REGEX MODE [VAR=VALUE...]
expect() {
  label="$1"; want="$2"; shift 2
  run "$@"
  if [ "$rc" = 0 ] && printf '%s\n' "$out" | grep -Eq -- "$want"; then ok; else bad "$label"; fi
}

# 1. Runaway recursion under a small limit: the Error, its catch semantics, the
#    trace, a future, and ten in a row with every throwable kept under a heap
#    ceiling only an unwound stack fits in.
expect "runaway under 64m" '^runaway ok$' runaway JOLT_STACK_LIMIT=64m JOLT_MAX_HEAP=384m
printf '%s\n' "$out" | grep '^elapsed-ms' | sed 's/^/stacklimit: runaway x12 under 64m: /'

# 1b. Under the default limit, in a process whose live heap is already a large share of its
#     ceiling: the stack overflows before the heap meets the ceiling, and the heap comes back.
expect "runaway beside a large live heap, default limit" '^ballast ok$' ballast JOLT_MAX_HEAP=600m

# 2. Legitimate depth under the default.
expect "1M frames under the default" '^1000000$' deep

# 3. -1, 0, off and none disable; the same recursion that overflows a small
#    limit completes.
for v in -1 0 off OFF none; do
  expect "$v reads back as no limit" '^nil$' limit JOLT_STACK_LIMIT=$v
done
expect "-1: 10M frames complete" '^10000000$' bounded JOLT_STACK_LIMIT=-1
expect "16m: 10M frames overflow" '^:stack-overflow$' bounded JOLT_STACK_LIMIT=16m

# 4. Spellings: k/m/g in either case, plain bytes.
expect "512k" '^524288$' limit JOLT_STACK_LIMIT=512k
expect "64M" '^67108864$' limit JOLT_STACK_LIMIT=64M
expect "2g" '^2147483648$' limit JOLT_STACK_LIMIT=2g
expect "1048576" '^1048576$' limit JOLT_STACK_LIMIT=1048576

# 5. A bad value is refused by name at startup, the way JOLT_MAX_HEAP's are.
for v in abc -2 1.5g 12x offf; do
  run limit JOLT_STACK_LIMIT="$v"
  if [ "$rc" != 0 ] && printf '%s' "$out" | grep -q "JOLT_STACK_LIMIT=$v is not valid"; then ok; else bad "invalid $v refused"; fi
done

# 6. The default is 80% of the heap ceiling in force; with no ceiling, of the
#    memory limit: RLIMIT_AS (Linux `ulimit -v`; macOS refuses to set it), else
#    RLIMIT_DATA, else physical RAM.
run limits
max=$(printf '%s\n' "$out" | head -1 | sed 's/^\[[0-9]* \([0-9]*\)\]$/\1/')
lim=$(printf '%s\n' "$out" | head -1 | sed 's/^\[\([0-9]*\) .*/\1/')
if [ "$rc" = 0 ] && [ -n "$max" ] && [ "$max" != 9223372036854775807 ] && [ "$lim" = "$((max * 4 / 5))" ]; then ok; else bad "default is 80% of the default heap ceiling"; fi
expect "JOLT_MAX_HEAP=1g -> 80% of 1g" '^\[858993459 1073741824\]$' limits JOLT_MAX_HEAP=1g
run limit JOLT_MAX_HEAP=off
def=$(printf '%s\n' "$out" | head -1)
case "$(uname -s)" in
  Darwin) phys=$(sysctl -n hw.memsize) ;;
  # Termux has no getconf; MemTotal is the same sysinfo total, in KiB.
  *) if command -v getconf >/dev/null 2>&1; then
       phys=$(( $(getconf _PHYS_PAGES) * $(getconf PAGE_SIZE) ))
     else
       phys=$(( $(sed -n 's/^MemTotal: *\([0-9]*\) kB$/\1/p' /proc/meminfo) * 1024 ))
     fi ;;
esac
if [ "$(ulimit -v)" != unlimited ] || [ "$(ulimit -d)" != unlimited ]; then
  echo "stacklimit: physical-RAM default skipped (this shell has a memory rlimit)"
elif [ "$rc" = 0 ] && [ "$def" = "$((phys * 4 / 5))" ]; then ok; else bad "JOLT_MAX_HEAP=off: default is 80% of physical RAM ($phys)"; fi
expect "an explicit limit ignores the ceiling" '^\[67108864 1073741824\]$' limits JOLT_MAX_HEAP=1g JOLT_STACK_LIMIT=64m
if [ "$(uname -s)" = Linux ] && (ulimit -v 16777216) 2>/dev/null; then
  out=$(ulimit -v 16777216; env JOLT_NO_USER_DEPS=1 JOLT_MAX_HEAP=off "$J" run "$T" limit 2>&1); rc=$?
  if [ "$rc" = 0 ] && [ "$(printf '%s\n' "$out" | head -1)" = "$((16777216 * 1024 * 4 / 5))" ]; then ok; else bad "JOLT_MAX_HEAP=off under ulimit -v 16G: 80% of RLIMIT_AS"; fi
else
  echo "stacklimit: RLIMIT_AS case skipped (cannot set ulimit -v here; the precedence is pinned in stack-limit-test.ss)"
fi

echo "stacklimit: $pass passed, $fail failed"
[ "$fail" = 0 ]
