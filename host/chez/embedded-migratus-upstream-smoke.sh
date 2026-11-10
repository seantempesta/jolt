#!/bin/sh
# Upstream Migratus discovers migrations inside a built binary's embedded
# resources. Migratus comes from the library-conformance checkout
# (JOLT_CONFORMANCE_LIBS, default ../conformance-libraries; see
# test/conformance/libs/README.md), not from Maven, so the check needs no
# network; like the library suites it skips when the checkout is absent.
set -eu
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
libs="${JOLT_CONFORMANCE_LIBS:-$root/../conformance-libraries}"
if [ ! -d "$libs/migratus/src/migratus" ]; then
  printf 'upstream Migratus discovery skipped: no migratus checkout under %s\n' "$libs"
  exit 0
fi
case "$libs" in /*) ;; *) libs="$root/$libs" ;; esac
jolt="${JOLT_BIN:-target/release/jolt}"
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT HUP INT TERM
app="$work/app"
cp -R "$root/test/chez/embedded-migratus-upstream" "$app"
cp -R "$root/test/chez/embedded-resource-app/resources" "$app/resources"
# the app's own src first: it stubs the namespaces Migratus loads for SQL
# migrations and logging, which reach for JDBC and a logging backend
printf '{:paths ["src" "resources" "%s/migratus/src"]\n :jolt/build {:embed ["resources"]}}\n' "$libs" > "$app/deps.edn"
JOLT_PWD="$app" "$joltabs" build -m check.core -o "$work/app-bin" >/dev/null 2>&1
got="$(cd / && "$work/app-bin")"
if [ "$got" != ':upstream-migratus-ok' ]; then
  echo "upstream Migratus discovery failed: $got" >&2
  exit 1
fi
printf 'upstream Migratus discovery passed (%s)\n' "$libs/migratus"
