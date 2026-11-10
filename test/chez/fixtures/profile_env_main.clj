;; Run under JOLT_PROFILE=1 by smoke.sh, with test/chez/fixtures on the source
;; roots: the fixture is REQUIRED, so a cached fasl of it from an earlier run
;; must not stand in for the instrumented compile. The report at exit counts it.
(require 'profile-fixture)
(profile-fixture/fib 10)
