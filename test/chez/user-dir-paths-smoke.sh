#!/bin/sh
# Relative paths under bin/jolt resolve against the CALLER's directory (#1241).
#
# bin/jolt exports the user's cwd as JOLT_PWD and then cd's to the checkout, so
# the runtime's own relative loads work. java.io, spit/slurp and ProcessBuilder
# already root a relative path at JOLT_PWD (io.ss project-relative), but the
# jolt.host filesystem functions, clojure.core/load-file and jolt.host/sh went
# to the process cwd — the checkout. (spit "a") then (jolt.host/delete-file!
# "a") touched two different files, which is how a stopped nREPL left
# .nrepl-port behind, and a bb.edn string task ran in the checkout. A built
# binary never cd's, so this only shows from a checkout, which is why it is a
# script driving bin/jolt from a scratch directory rather than a unit row.

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
jolt="$root/bin/jolt"

work="$(mktemp -d "${TMPDIR:-/tmp}/jolt-user-dir-XXXXXX")"
trap 'rm -rf "$work"' EXIT
cd "$work"

check() {
  clabel="$1"; cgot="$2"; cwant="$3"
  if [ "$cgot" = "$cwant" ]; then
    echo "PASS: ($clabel) $cgot"; pass=$((pass+1))
  else
    echo "FAIL: ($clabel) got '$cgot', want '$cwant'"; fails=$((fails+1))
  fi
}

# run <expr>: evaluate from $work and print the LAST line of stdout.
run() { JOLT_QUIET=1 "$jolt" -e "$1" </dev/null 2>/dev/null | tail -1; }

check "file-exists?/directory? see the caller's file" \
  "$(run '(do (spit "a.txt" "1") [(jolt.host/file-exists? "a.txt") (jolt.host/directory? "sub") (do (.mkdir (java.io.File. "sub")) (jolt.host/directory? "sub"))])')" \
  "[true false true]"

check "list-dir . lists the caller's directory" \
  "$(run '(boolean (some #{"a.txt"} (jolt.host/list-dir ".")))')" \
  "true"

check "file-mtime reads the caller's file" \
  "$(run '(pos? (jolt.host/file-mtime "a.txt"))')" \
  "true"

check "rename-file! moves the caller's file" \
  "$(run '[(jolt.host/rename-file! "a.txt" "b.txt") (.exists (java.io.File. "a.txt")) (.exists (java.io.File. "b.txt"))]')" \
  "[true false true]"

check "delete-file! deletes the caller's file" \
  "$(run '[(jolt.host/delete-file! "b.txt") (.exists (java.io.File. "b.txt"))]')" \
  "[true false]"

check "io/delete-file deletes the caller's file" \
  "$(run '(do (require (quote [clojure.java.io :as io])) (spit "c.txt" "1") (prn [(io/delete-file "c.txt") (.exists (java.io.File. "c.txt"))]))')" \
  "[true false]"

check "mkdirs! creates in the caller's directory" \
  "$(run '[(jolt.host/mkdirs! "zz/yy") (.isDirectory (java.io.File. "zz/yy"))]')" \
  "[true true]"

ln -s sub link
check "symlink? sees the caller's link" \
  "$(run '[(jolt.host/symlink? "link") (jolt.host/symlink? "sub")]')" \
  "[true false]"

check "delete-tree! removes the caller's tree" \
  "$(run '[(jolt.host/delete-tree! "zz") (.exists (java.io.File. "zz"))]')" \
  "[true false]"

echo '(def loaded-here :yes)' > here.clj
check "load-file reads the caller's file" \
  "$(run '(do (load-file "here.clj") (prn loaded-here))')" \
  ":yes"

# A shell child starts in the caller's directory, and without JOLT_PWD, which
# names THIS process's directory: inherited, it would hand a child jolt the
# parent's project.
check "sh runs in the caller's directory" \
  "$(run '(prn (jolt.host/sh "test -f here.clj"))')" \
  "0"
check "sh-out runs there, without JOLT_PWD" \
  "$(run '(print (jolt.host/sh-out "echo \"$(pwd)|${JOLT_PWD:-unset}\""))')" \
  "$(pwd)|unset"

printf '{:tasks {where "pwd"}}\n' > bb.edn
check "a bb.edn string task runs in the project" \
  "$(JOLT_QUIET=1 "$jolt" where </dev/null 2>/dev/null | tail -1)" \
  "$(pwd)"
rm -f bb.edn

# The nREPL stop fn deletes the .nrepl-port start wrote, here.
check "nREPL stop removes the caller's .nrepl-port" \
  "$(run '(do (require (quote jolt.nrepl)) (let [stop (binding [*out* (java.io.StringWriter.)] ((resolve (quote jolt.nrepl/start)) 0)) written (.exists (java.io.File. ".nrepl-port"))] (stop) (prn [written (.exists (java.io.File. ".nrepl-port"))])))')" \
  "[true false]"

# jolt's own install roots ("stdlib", "jolt-core") stay relative to the
# checkout, where the resource was found; its URL used to be absolutized
# against the caller's directory instead, naming a file that isn't there.
# Source mode by name: once `make devboot` has written target/dev/flat.so,
# bin/jolt loads that image, whose resources are embedded rather than files,
# so these checks failed or passed depending on whether devboot ran first.
runsrc() { JOLT_NO_DEVCACHE=1 run "$1"; }
check "io/resource of a stdlib file names a file that exists" \
  "$(runsrc '(do (require (quote [clojure.java.io :as io])) (let [u (io/resource "clojure/string.clj")] (prn [(.exists (io/file u)) (pos? (count (slurp u)))])))')" \
  "[true true]"
check "io/resource of a compiler file names a file that exists" \
  "$(runsrc '(do (require (quote [clojure.java.io :as io])) (prn (.exists (io/file (io/resource "jolt/analyzer.clj")))))')" \
  "true"

# Nothing landed in the checkout.
leaked=""
for f in a.txt b.txt c.txt zz sub link here.clj .nrepl-port; do
  if [ -e "$root/$f" ] || [ -L "$root/$f" ]; then leaked="$leaked $f"; fi
done
check "nothing written into the checkout" "${leaked:-none}" "none"

echo ""
echo "user-dir-paths smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
