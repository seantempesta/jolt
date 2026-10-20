(ns jolt.time.impl
  "Machinery shared by the java.time value types.

  A value is an opaque host table tagged with its type keyword and carrying its
  canonical fields. Equality, hashing, str/pr rendering, ordering, and instance?
  are supplied per type through jolt's value-semantics seams, so a java.time
  value compares, prints, and sorts like the real thing without being a map."
  (:require [jolt.host :as host]))

;; --- civil <-> days since the Unix epoch (Howard Hinnant's algorithms) -------
;; No portable UTC mktime, so epoch days are computed directly from y/m/d.

(defn days-from-civil
  "Days from 1970-01-01 to the proleptic-Gregorian date y/m/d."
  [y m d]
  (let [y2  (if (<= m 2) (dec y) y)
        era (quot (if (>= y2 0) y2 (- y2 399)) 400)
        yoe (- y2 (* era 400))
        doy (+ (quot (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn civil-from-days
  "Inverse of days-from-civil; returns [year month day]."
  [z]
  (let [z2  (+ z 719468)
        era (quot (if (>= z2 0) z2 (- z2 146096)) 146097)
        doe (- z2 (* era 146097))
        yoe (quot (+ doe (- (quot doe 1460)) (quot doe 36524) (- (quot doe 146096))) 365)
        y   (+ yoe (* era 400))
        doy (- doe (+ (* 365 yoe) (quot yoe 4) (- (quot yoe 100))))
        mp  (quot (+ (* 5 doy) 2) 153)
        d   (+ (- doy (quot (+ (* 153 mp) 2) 5)) 1)
        m   (+ mp (if (< mp 10) 3 -9))]
    [(if (<= m 2) (inc y) y) m d]))

;; --- value machinery ---------------------------------------------------------

;; type-kw -> {:eq (fn [a b]) :hash (fn [v]) :str (fn [v]) :cmp (fn [a b]|nil)
;;             :classes #{class-name-strings}}
(defonce ^:private registry (atom {}))

(def ^:private type-key :jolt.time/type)

(defn register-type!
  "Register value semantics for a java.time type keyword. :cmp may be nil for an
  unordered type; :classes lists every class/interface name the type satisfies
  (its own plus supertypes like Temporal/Comparable), simple and fully qualified."
  [type-kw spec]
  (swap! registry assoc type-kw spec))

(defn value
  "An opaque value of `type-kw` carrying `fields` (a map)."
  [type-kw fields]
  (let [t (host/tagged-table type-kw)]
    (host/ref-put! t type-key type-kw)
    (reduce-kv (fn [_ k v] (host/ref-put! t k v)) nil fields)
    t))

(defn field [v k] (host/ref-get v k))
(defn type-of [v] (and (host/table? v) (host/ref-get v type-key)))

(defn clock-millis
  "Current epoch millis honoring a Clock argument (system/fixed/offset/tick), or
  the wall clock for nil / a non-clock. tick's `now`/`today` pass *clock* here."
  [c]
  (if (and (host/table? c) (= :jolt.time/clock (host/ref-get c type-key)))
    (case (field c :kind)
      :fixed (field c :ms)
      :offset (+ (clock-millis (field c :base)) (field c :ms))
      :tick (let [m (clock-millis (field c :base)) dm (field c :ms)] (if (pos? dm) (* (quot m dm) dm) m))
      (System/currentTimeMillis))
    (System/currentTimeMillis)))

;; java.time's compareTo answers the first field difference, a magnitude, not
;; its sign: LocalDate's is the year difference, then the month's, then the
;; day's. The :cmp fns build that from the per-field differences in order.
(defn first-nonzero [& cs] (or (some #(when-not (zero? %) %) cs) 0))

;; Instant and Duration: Long.compare on the seconds, then the nano difference.
(defn secs-nanos-cmp [a b]
  (let [na (mod a 1000000000) nb (mod b 1000000000)
        sa (quot (- a na) 1000000000) sb (quot (- b nb) 1000000000)]
    (if (= sa sb) (- na nb) (compare sa sb))))

(defn jt?
  "Is v one of our java.time values?"
  [v]
  (boolean (when-let [tk (type-of v)] (contains? @registry tk))))

(defn- spec-of [v] (get @registry (type-of v)))

(defn- same-type? [a b]
  (and (jt? a) (jt? b) (= (type-of a) (type-of b))))

;; --- the seams (installed once) ----------------------------------------------

;; The class name (class x)/(type x) reports per time type, and what protocol
;; dispatch keys on. Defined before install-seams!, which closes over it.
(def ^:private type->class
  {:jolt.time/month "java.time.Month" :jolt.time/day-of-week "java.time.DayOfWeek"
   :jolt.time/chrono-unit "java.time.temporal.ChronoUnit" :jolt.time/chrono-field "java.time.temporal.ChronoField"
   :jolt.time/value-range "java.time.temporal.ValueRange"
   :jolt.time/local-date "java.time.LocalDate" :jolt.time/local-time "java.time.LocalTime"
   :jolt.time/local-date-time "java.time.LocalDateTime"
   :jolt.time/duration "java.time.Duration" :jolt.time/period "java.time.Period"
   :jolt.time/year "java.time.Year" :jolt.time/year-month "java.time.YearMonth" :jolt.time/month-day "java.time.MonthDay"
   :jolt.time/instant "java.time.Instant"
   :jolt.time/zone-offset "java.time.ZoneOffset" :jolt.time/zone-id "java.time.ZoneId"
   :jolt.time/zone-rules "java.time.zone.ZoneRules"
   :jolt.time/zoned-date-time "java.time.ZonedDateTime" :jolt.time/offset-date-time "java.time.OffsetDateTime"
   :jolt.time/offset-time "java.time.OffsetTime"
   :jolt.time/temporal-adjuster "java.time.temporal.TemporalAdjuster"
   :jolt.time/dt-formatter "java.time.format.DateTimeFormatter"
   :jolt.time/locale "java.util.Locale" :jolt.time/format-style "java.time.format.FormatStyle"
   :jolt.time/dtf-builder "java.time.format.DateTimeFormatterBuilder"
   :jolt.time/clock "java.time.Clock"})

(defn- install-seams! []
  ;; every java.time value is a host table (type-of), so each arm claims tables
  ;; only: the runtime then never calls jt? for a value of another kind
  (let [claim {:kind :table}]
    (__register-eq!
     (fn [a b] (or (jt? a) (jt? b)))
     (fn [a b] (boolean (and (same-type? a b) ((:eq (spec-of a)) a b))))
     claim)
    (__register-hash! jt? (fn [v] ((:hash (spec-of v)) v)) claim)
    ;; java.time values have no reader literal, so pr renders the same as str
    ;; (time-literals layers #time/… readable printing on top separately).
    (__register-str! jt? (fn [v] ((:str (spec-of v)) v)) claim)
    (__register-pr!  jt? (fn [v] ((:str (spec-of v)) v)) claim)
    (__register-compare!
     (fn [a b] (or (jt? a) (jt? b)))
     (fn [a b]
       ;; compareTo's cast refuses a value of another type, and a type with no
       ;; order is not Comparable at all: both are ClassCastExceptions on the JVM
       (let [cmp (:cmp (spec-of a))]
         (when-not (and cmp (same-type? a b))
           (throw (jolt.host/throwable "java.lang.ClassCastException"
                                       (str (pr-str a) " cannot be compared to " (pr-str b)))))
         (cmp a b)))
     claim)
    ;; java.lang.Object is answered here too, not just by the host's root rule: an
    ;; arm returns a DEFINITIVE false for any class the value's set does not list,
    ;; and a host whose root rule is consulted after the arms would otherwise read
    ;; that false and make (instance? Object v) — and (.cast Object v), which every
    ;; SCI interop argument goes through — fail (#985). Both spellings: Class.cast
    ;; passes "java.lang.Object", (instance? Object x) passes "Object".
    (__register-instance-check!
     (fn [class-name v]
       (when (jt? v)
         (boolean (or (contains? (:classes (spec-of v)) class-name)
                      (contains? #{"java.io.Serializable" "Serializable"
                                   "java.lang.Object" "Object"} class-name)))))
     claim)
    ;; (class x)/(type x) and — crucially — protocol dispatch on these values, which
    ;; keys on value-host-tags. Without this a value's class is :object and
    ;; (extend-protocol P java.time.X …) never fires (tick extends its protocols this way).
    (__register-class!
     jt?
     (fn [x] (get type->class (type-of x) "java.lang.Object"))
     (fn [x] (conj (vec (:classes (spec-of x))) "java.io.Serializable" "Serializable"))
     claim)))

(defonce ^:private installed (install-seams!))
