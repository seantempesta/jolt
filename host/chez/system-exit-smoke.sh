#!/bin/sh
# System/exit ends the process with the status it was given, as on the JVM (Runtime.exit: the
# shutdown hooks run, then the process halts): a `finally` the call is inside never runs, so it
# cannot print or exit again over that status. Unfixed, the exit unwound to the program's top
# level and every `finally` on the way ran: a Seon agent's (System/exit 3) reached root as exit
# 1, the status its forked process's own boundary set in a `finally`.
# JOLT_BIN: the jolt binary (default target/release/jolt).
JOLT=${JOLT_BIN:-target/release/jolt}
pass=0; fail=0
check() { # NAME WANT-STATUS WANT-OUT FORM
  out=$(JOLT_NO_USER_DEPS=1 "$JOLT" -e "$4" 2>/dev/null); st=$?
  if [ "$st" = "$2" ] && [ "$out" = "$3" ]; then pass=$((pass+1)); else fail=$((fail+1)); echo "FAIL $1: status $st (want $2), out [$out] (want [$3])"; fi
}
check "a finally around System/exit does not run" 3 "" '(try (System/exit 3) (finally (println "finally ran") (System/exit 1)))'
check "nor on the main carrier" 3 "" '(jolt.host/main-carrier-run! (fn [] (try (System/exit 3) (finally (println "finally ran") (System/exit 1)))))'
check "a shutdown hook runs" 4 "hook ran" '(.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (println "hook ran")))) (System/exit 4)'
check "output written before it is flushed" 5 "written" '(print "written") (System/exit 5)'
check "from a thread the program started, with its hook run" 6 "hook ran" '(.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (println "hook ran")))) (future (try (System/exit 6) (finally (println "finally ran")))) @(promise)'
echo "system-exit-smoke: $pass passed, $fail failed"
[ "$fail" = 0 ]
