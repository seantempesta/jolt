#!/bin/sh
# A shutdown signal names its sender: our Chez's SA_SIGINFO handler hands Jolt's watcher {sig pid uid}, and the watcher
# writes "jolt: signal N from pid P uid U" on stderr before the hooks run (an agent died of a HUP nobody named). Here a
# jolt process that sleeps is sent SIGHUP from this shell: its stderr names this shell's pid and it exits 129.
# Run: sh test/chez/signal-sender-test.sh   (prints SIGNAL-SENDER-TEST OK)
err=$(mktemp)
JOLT_NO_USER_DEPS=1 bin/jolt -e '(Thread/sleep 30000)' 2> "$err" &
child=$!
sleep 8
kill -HUP "$child"
wait "$child"; status=$?
if ! grep -q "jolt: signal 1 from pid $$ " "$err"; then echo "FAIL signal-sender: stderr does not name pid $$:"; cat "$err"; rm -f "$err"; exit 1; fi
if [ "$status" != 129 ]; then echo "FAIL signal-sender: exit $status, not 129"; rm -f "$err"; exit 1; fi
rm -f "$err"
echo "SIGNAL-SENDER-TEST OK"
