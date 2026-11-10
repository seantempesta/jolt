;; Run under JOLT_PROFILE=sample by smoke.sh, with test/chez/fixtures on the
;; source roots. The report at exit has to name the fixture's loop.
(ns sample-env-main
  (:require [sample-fixture :as f]))

(f/driver 10000000)
