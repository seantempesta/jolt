#!/bin/sh
set -eu

root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
jolt="${JOLT_BIN:-target/release/jolt}"
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac
app="$root/test/chez/embedded-resource-app"
out="$(mktemp -d)/app-bin"
trap 'rm -rf "$(dirname "$out")"' EXIT HUP INT TERM

JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >/dev/null 2>&1
run_mode() {
  mode="$1"
  run_dir=/
  if [ "$mode" = "--nio-read" ]; then
    run_dir="$(dirname "$out")/cwd"
    mkdir -p "$run_dir/migrations"
    printf 'wrong host file\n' > "$run_dir/migrations/001-create-users.up.sql"
  fi
  if [ "$mode" = "--jar" ]; then
    run_dir="$(dirname "$out")/jar-cwd"
    mkdir -p "$run_dir"
    printf 'not a zip archive\n' > "$run_dir/jolt-embedded-resources.jar"
  fi
  got="$(cd "$run_dir" && "$out" "$mode")"

  case "$mode" in
    --names)
      want='["greeting.txt" "migrations-old/ignore.sql" "migrations/001-create-users.down.sql" "migrations/001-create-users.up.sql" "migrations/init.sql" "migrations/nested/002-add-email.up.sql"]'
      ;;
    --jar) want=':jar-ok' ;;
    --nio) want=':nio-ok' ;;
    --nio-read) want=':nio-read-ok' ;;
    --migratus) want=':migratus-ok' ;;
    *)
      echo "embedded resource smoke: unknown mode $mode" >&2
      exit 2
      ;;
  esac

  if [ "$got" != "$want" ]; then
    echo "embedded resource smoke: $mode failed" >&2
    echo "--- want ---" >&2
    printf '%s\n' "$want" >&2
    echo "--- got ----" >&2
    printf '%s\n' "$got" >&2
    exit 1
  fi
  printf 'embedded resource smoke: %s passed\n' "$mode"
}

if [ "$#" -eq 0 ] || [ "$1" = "--all" ]; then
  for mode in --names --jar --nio --nio-read --migratus; do
    run_mode "$mode"
  done
else
  run_mode "$1"
fi
