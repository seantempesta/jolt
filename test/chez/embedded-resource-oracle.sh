#!/bin/sh
# The embedded archive against the JDK, without a JDK.
#
# The JVM's answers for the fixture app's resources (a real jar built from
# test/chez/embedded-java-app/resources, read through JarFile, its versions,
# its Manifest and a URLClassLoader) are recorded in
# test/chez/embedded-resource-oracle.edn. The gate builds the app and diffs its
# --oracle output against that record, so it runs wherever jolt does.
#
#   sh test/chez/embedded-resource-oracle.sh            compare against the record
#   sh test/chez/embedded-resource-oracle.sh --record   re-record from the JDK (jar,
#                                                       javap and clojure required)
#                                                       and audit the API coverage
set -eu
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
expected="test/chez/embedded-resource-oracle.edn"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT HUP INT TERM

if [ "${1:-}" = "--record" ]; then
  for tool in jar javap clojure; do
    if ! command -v "$tool" >/dev/null 2>&1; then
      echo "embedded resource oracle: --record needs $tool on PATH" >&2
      exit 2
    fi
  done
  resources="test/chez/embedded-java-app/resources"
  jar="$work/embedded-resource-oracle.jar"
  (cd "$resources" && jar --create --file "$jar" --manifest META-INF/MANIFEST.MF .)
  clojure -Sdeps '{:paths ["test/chez"]}' -M -e "(require 'embedded-resource-oracle) (embedded-resource-oracle/-main \"$jar\")" > "$work/jvm.edn"
  cp "$work/jvm.edn" "$expected"
  printf 'embedded resource oracle recorded to %s\n' "$expected"
  exit 0
fi

jolt="${JOLT_BIN:-target/release/jolt}"
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac
app="$root/test/chez/embedded-java-app"
JOLT_PWD="$app" "$joltabs" build -m app.core -o "$work/app-bin" >/dev/null 2>&1
(cd / && "$work/app-bin" --oracle) > "$work/jolt.edn"
if ! diff -u "$expected" "$work/jolt.edn"; then
  echo "embedded resource oracle: jolt differs from the recorded JDK answers" >&2
  exit 1
fi
printf 'embedded resource oracle matches the recorded JDK answers\n'
