;; Run under JOLT_PROFILE=debug by smoke.sh, with test/chez/fixtures on the source
;; roots: the fixture is REQUIRED, so a cached fasl of it from an earlier run
;; must not stand in for the debug compile. Prints the throwing frame's locals.
(require 'debug-fixture '[jolt.debug :as dbg])
(prn (:locals (first (dbg/frames (try (debug-fixture/outer 20) (catch Throwable t t))))))
