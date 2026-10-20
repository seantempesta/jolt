#!/bin/sh
# fork-reprotect-test.sh — a fork()ed child returns into Scheme code and runs.
# Invoke from the repo root:
#   sh test/chez/fork-reprotect-test.sh [jolt] [chez]
#
# On macOS arm64 XNU gives a forked child the forking thread's MAP_JIT view
# (pthread_jit_write_protect_np) as the kernel last saved it at a context
# switch, not its live value. A thread descheduled inside a collection (code
# writable) that re-protected without another switch forks a child whose code
# is not executable, and the child dies of SIGBUS ("invalid memory reference")
# on its first Scheme instruction. Both rows fork 2000 children with a full
# collection in the parent after each; before the fix about 6% of them died.
#   - the jolt binary: its main registers the child handler
#     (build.ss bld-fork-reprotect-defn), so it holds on any kernel.
#   - the kernel alone: chez-patches/0006 registers it in S_thread_init. On a
#     kernel without jolt's patches (the lockfree entry is the marker, they
#     ship together) the row reports and passes, unless
#     JOLT_REQUIRE_LOCKFREE_CHEZ=1.
# Elsewhere both rows are a plain fork smoke. There is no fork on Windows.
set -eu

J=${1:-target/release/jolt}
case "$J" in /*) ;; *) J="$(cd "$(dirname "$J")" && pwd)/$(basename "$J")" ;; esac
CHEZ=${2:-${CHEZ:-}}
N=${JOLT_FORK_REPROTECT_N:-2000}
here=$(cd "$(dirname "$0")" && pwd)

case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*|Windows_NT) echo "fork-reprotect: no fork() on Windows, skipped"; exit 0 ;;
esac

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fail=0

# --- the jolt binary ------------------------------------------------------------
printf '%s\n' "(println (jolt.host/scheme-eval-string \"(begin (load \\\"$here/fork-reprotect.ss\\\") (fork-reprotect-run $N))\"))" > "$tmp/probe.clj"
# a dead child's report goes to the inherited stderr; the count is the result
got=$(JOLT_NO_USER_DEPS=1 "$J" "$tmp/probe.clj" 2>"$tmp/jolt.err" | tail -n 1)
echo "fork-reprotect: jolt binary, $N forks: $got failed"
if [ "$got" != 0 ]; then
  echo "FAIL: forked children of the jolt binary died (first report below)" >&2
  head -n 2 "$tmp/jolt.err" >&2
  fail=1
fi

# --- the kernel alone -----------------------------------------------------------
if [ -n "$CHEZ" ]; then
  printf '%s\n' "(load \"$here/fork-reprotect.ss\")" \
    "(printf \"~a ~a~%\" (foreign-entry? \"(cs)lockfree_activation\") (fork-reprotect-run $N))" > "$tmp/probe.ss"
  line=$("$CHEZ" --script "$tmp/probe.ss" 2>"$tmp/chez.err" | tail -n 1)
  patched=${line% *}
  got=${line#* }
  echo "fork-reprotect: kernel alone, $N forks: $got failed (patched kernel: $patched)"
  if [ -z "$got" ] || [ "$got" = "$line" ]; then
    echo "FAIL: the kernel-alone probe printed no result" >&2
    head -n 2 "$tmp/chez.err" >&2
    fail=1
  elif [ "$got" != 0 ]; then
    if [ "$patched" = "#t" ] || [ "${JOLT_REQUIRE_LOCKFREE_CHEZ:-}" = 1 ]; then
      echo "FAIL: forked children of the kernel alone died: chez-patches/0006 is not in it" >&2
      fail=1
    else
      echo "fork-reprotect: stock Chez kernel, the kernel row is not checked"
    fi
  fi
fi

[ "$fail" = 0 ] && echo "fork-reprotect: passed"
exit "$fail"
