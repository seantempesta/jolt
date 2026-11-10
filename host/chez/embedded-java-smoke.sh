#!/bin/sh
set -eu
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
jolt="${JOLT_BIN:-target/release/jolt}"
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac
app="$root/test/chez/embedded-java-app"
if [ "${1:-}" = "--jar-base" ]; then
  app="$root/test/chez/embedded-java-base-app"
fi
out="$(mktemp -d)/app-bin"
trap 'rm -rf "$(dirname "$out")"' EXIT HUP INT TERM
JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >/dev/null 2>&1
mode="${1:---url}"
run_dir=/
if [ "$mode" = "--files-full" ]; then
  run_dir="$(dirname "$out")/cwd"
  mkdir -p "$run_dir"
fi
got="$(cd "$run_dir" && "$out" "$mode")"
case "$mode" in
  --url) want=':url-ok' ;;
  --resource-url) want=':resource-url-ok' ;;
  --jar-full) want=':jar-full-ok' ;;
  --jar-base) want=':jar-base-ok' ;;
  --fs-full) want=':fs-full-ok' ;;
  --files-full) want=':files-full-ok' ;;
  --archive-lifecycle) want=':archive-lifecycle-ok' ;;
  *) echo "embedded java smoke: unknown mode $mode" >&2; exit 2 ;;
esac
if [ "$got" != "$want" ]; then
  echo "embedded java smoke: $mode failed" >&2
  echo "--- want ---" >&2; printf '%s\n' "$want" >&2
  echo "--- got ----" >&2; printf '%s\n' "$got" >&2
  exit 1
fi
printf 'embedded java smoke: %s passed\n' "$mode"
