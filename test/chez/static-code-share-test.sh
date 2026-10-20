#!/bin/sh
# static-code-share-test.sh — a fork()ed child shares the parent's static code
# (macOS arm64; chez-patches/0007). Invoke from the repo root:
#   sh test/chez/static-code-share-test.sh [chez]
#
# fork() copies every resident page of a MAP_JIT mapping into the child, so a
# child paid its parent's whole code heap at the fork. With
# CHEZ_SHARE_STATIC_CODE=1 the kernel moves static code out of MAP_JIT after a
# collection into the static generation, and the child shares it
# copy-on-write. The probe (static-code-share.ss) runs with the variable 0 and
# 1 and reads the child's resident executable memory held privately; with
# sharing on it must be under half of what it is with sharing off (Chez alone,
# measured: 11680 KB off, 1360 KB on). On a kernel without jolt's patches (the
# lockfree entry is the marker, they ship together) the row reports and
# passes, unless JOLT_REQUIRE_LOCKFREE_CHEZ=1. Elsewhere fork() already shares
# code copy-on-write, and the test is skipped.
set -eu

CHEZ=${1:-${CHEZ:-scheme}}
here=$(cd "$(dirname "$0")" && pwd)

if [ "$(uname -s)" != Darwin ] || [ "$(uname -m)" != arm64 ]; then
  echo "static-code-share: not macOS arm64, skipped"
  exit 0
fi
cc=${JOLT_CC:-${CC:-cc}}
command -v "$cc" >/dev/null 2>&1 || { echo "static-code-share: no C compiler, skipped"; exit 0; }

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
"$cc" -O1 -dynamiclib -o "$tmp/helper.dylib" "$here/static-code-share-helper.c"

off=$(CHEZ_SHARE_STATIC_CODE=0 "$CHEZ" --script "$here/static-code-share.ss" "$tmp/helper.dylib" | tail -n 1)
on=$(CHEZ_SHARE_STATIC_CODE=1 "$CHEZ" --script "$here/static-code-share.ss" "$tmp/helper.dylib" | tail -n 1)
patched=$(echo "$on" | awk '{print $1}')
priv_off=$(echo "$off" | awk '{print $2}')
priv_on=$(echo "$on" | awk '{print $2}')
echo "static-code-share: child's private executable KB, sharing off $priv_off, on $priv_on (patched kernel: $patched)"
case "$priv_off$priv_on" in
  *[!0-9]*|'') echo "FAIL: the probe printed no result" >&2; exit 1 ;;
esac
if [ $((priv_on * 2)) -ge "$priv_off" ]; then
  if [ "$patched" = "#t" ] || [ "${JOLT_REQUIRE_LOCKFREE_CHEZ:-}" = 1 ]; then
    echo "FAIL: the child copied the parent's static code: chez-patches/0007 is not in the kernel" >&2
    exit 1
  fi
  echo "static-code-share: stock Chez kernel, not checked"
  exit 0
fi
echo "static-code-share: passed"
