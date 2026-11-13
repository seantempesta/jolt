#!/bin/sh
# A namespace loaded from the AOT cache brings its line-marker table with it, as a
# built binary's unit does (build.ss bld-write-unit!, loader.ss aot-publish-artifact!):
# a frame through it finds its line in the registered table, never by reading and
# scanning the namespace's whole .scm on the first trace through it. Before, that scan
# was the first trace's cost (seon.config, 2.6 MB of .scm: 1.37M ticks first, 18k
# after), paid by whichever trace came first, so a forked process's first trace cost
# whatever its parent had not yet indexed.
#
# The first run compiles mylib.big and publishes it; the second loads it from the
# cache and reads the registered table, which must equal the table a scan of the
# cached .scm gives (the same offsets, the same lines).
set -e
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
jolt="${JOLT_BIN:-bin/jolt}"
tmp="$(mktemp -d)"; cache="$tmp/cache"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/src/mylib"
{ echo "(ns mylib.big)"
  i=0; while [ $i -lt 200 ]; do echo "(defn f$i [x] (let [y (* x $i)] (if (pos? y) (+ y $i) (- y $i))))"; i=$((i+1)); done
  echo '(defn boom [] (throw (ex-info "boom" {})))'; } > "$tmp/src/mylib/big.clj"
run() {
  JOLT_AOT_CACHE=1 JOLT_AOT_ASYNC=0 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps 'jolt.scheme)
    (jolt.deps/add-deps {:deps {'mylib/mylib {:local/root \"$tmp\"}}})
    (require 'mylib.big)
    (let [scm (first (filter #(.endsWith (str %) \".scm\") (file-seq (java.io.File. \"$cache\"))))
          eq (jolt.scheme/eval-string
               (str \"(let ((t (jolt-baked-marker-table \\\"mylib.big\\\")))\"
                    \"  (and t (fx>? (vector-length t) 0)\"
                    \"       (equal? t (jolt-marker-table (call-with-input-file \" (pr-str (str scm)) \" get-string-all)))))\"))]
      (println (if eq \"TABLE-OK\" \"NO-TABLE\")))" 2>&1 | tail -1
}
run >/dev/null
out="$(run)"
if [ "$out" = "TABLE-OK" ]; then
  echo "aot-frame-table: a cache-loaded namespace registered its marker table, equal to its .scm's: OK"
else
  echo "aot-frame-table: FAIL (second run printed: $out)"; exit 1
fi
