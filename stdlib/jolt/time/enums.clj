(ns jolt.time.enums
  "java.time enum + field types: Month, DayOfWeek, ChronoUnit, ChronoField, and
  ValueRange. Requiring the namespace registers the classes. The temporal-coupled
  operations (Month/from, ChronoUnit.between, ChronoField.getFrom, …) are added by
  the temporal namespaces, which own the receiver types."
  (:require [clojure.string :as str]
            [jolt.time.impl :as impl]
            [jolt.time.util :as u]))

(defn- statics! [names members]
  (doseq [n names] (__register-class-statics! n members)))

;; The four enums are declared through jolt.host/register-enum!, which gives them
;; values/valueOf, Class.isEnum, Enum/valueOf and the JVM's print form. A
;; constant is ONE value, so every constructor below hands back the declared
;; constant rather than building an equal one.
(defn- enum! [fqn names consts]
  (jolt.host/register-enum! fqn (mapv vector names consts)))

;; A ChronoUnit's toString: its name in camel case (HALF_DAYS is HalfDays), as
;; the JDK declares it.
(defn- display-name [nm]
  (apply str (map (fn [part]
                    (str (subs part 0 1) (.toLowerCase (subs part 1))))
                  (.split nm "_"))))

;; ---- localized names -----------------------------------------------------------
;; Month / DayOfWeek .getDisplayName(TextStyle, Locale) and ChronoField
;; .getDisplayName(Locale), from the :date-names and :field-names extension
;; points: core carries ROOT, jolt-lang/time the other locales (RFC 0008). A
;; Locale renders as its id. A provider without narrow forms gets the format
;; name's first letter, which is CLDR's narrow form in every locale measured;
;; one without standalone forms gets the format forms.
(defn- locale-id [l]
  (when (nil? l) (throw (jolt.host/throwable "java.lang.NullPointerException" "locale")))
  ;; the data's key is the BCP-47-style id (language-Script-COUNTRY-variant); a
  ;; Locale prints the JDK's toString (en_US, zh_TW_#Hant)
  (let [s (str l) h (.indexOf s "_#")
        base (if (neg? h) s (subs s 0 h))
        script (if (neg? h) "" (subs s (+ h 2)))
        [lang & more] (str/split base #"_" -1)]
    (str/join "-" (remove empty? (concat [lang script] more)))))
(defn- text-name [kind i style l]
  (when (nil? style) (throw (jolt.host/throwable "java.lang.NullPointerException" "style")))
  (let [id (locale-id l)
        d (jolt.host/extension-value :date-names id)
        st (str style)
        standalone? (.endsWith st "_STANDALONE")
        k (fn [suffix] (keyword (str (name kind) suffix)))
        pick (fn [ks] (some (fn [key] (let [v (get d key)] (when v (nth v i nil)))) ks))]
    (cond
      (.startsWith st "FULL") (pick (if standalone? [(k "-standalone") kind] [kind]))
      (.startsWith st "SHORT") (pick (if standalone? [(k "-short-standalone") (k "-short")] [(k "-short")]))
      :else (or (pick [(k "-narrow")])
                (if (and (= id "") (= kind :months))
                  (str (inc i))
                  (let [full (pick [kind])] (.toUpperCase (subs full 0 1))))))))

;; ---- Month ------------------------------------------------------------------

(def ^:private months (mapv #(impl/value :jolt.time/month {:value %}) (range 1 13)))
(defn month [n]
  (or (get months (dec n))
      (throw (jolt.host/throwable "java.time.DateTimeException"
                                  (str "Invalid value for MonthOfYear: " n)))))
(defn month-val [e] (impl/field e :value))
(defn- month-name [e] (nth u/month-names (dec (month-val e))))
;; A month argument: the JDK overloads every `of` that takes a month number with
;; one that takes a Month (LocalDate, LocalDateTime, YearMonth, MonthDay).
(defn month-num [m]
  (if (= :jolt.time/month (impl/type-of m)) (month-val m) (u/->long m)))

;; The four enums below order as Enum.compareTo does: by the ordinal
;; DIFFERENCE, not its sign, so compare and .compareTo answer the same number.
(impl/register-type! :jolt.time/month
  {:eq   (fn [a b] (= (month-val a) (month-val b)))
   :hash (fn [e] (month-val e))
   :str  month-name
   :enum? true
   :cmp  (fn [a b] (- (month-val a) (month-val b)))
   :classes #{"java.time.Month" "Month"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})

(__register-class-methods! :jolt.time/month
  {"getValue"  (fn [e] (month-val e))
   "ordinal"   (fn [e] (dec (month-val e)))
   "name"      month-name
   "toString"  month-name
   "getDisplayName" (fn [e style l] (text-name :months (dec (month-val e)) style l))
   "plus"      (fn [e n] (month (inc (u/floor-mod (+ (dec (month-val e)) (u/->long n)) 12))))
   "minus"     (fn [e n] (month (inc (u/floor-mod (- (dec (month-val e)) (u/->long n)) 12))))
   "length"    (fn [e leap] (u/len-of-month (if leap 4 1) (month-val e)))
   "minLength" (fn [e] (if (= 2 (month-val e)) 28 (u/len-of-month 1 (month-val e))))
   "maxLength" (fn [e] (if (= 2 (month-val e)) 29 (u/len-of-month 1 (month-val e))))
   "firstMonthOfQuarter" (fn [e] (month (inc (* 3 (quot (dec (month-val e)) 3)))))
   "compareTo" compare
   "equals"    (fn [e o] (boolean (and (impl/jt? o) (= :jolt.time/month (impl/type-of o))
                                        (= (month-val e) (month-val o)))))
   "hashCode"  (fn [e] (month-val e))})

(enum! "java.time.Month" u/month-names months)
(statics! ["Month" "java.time.Month"] {"of" (fn [n] (month (u/->long n)))})

;; ---- DayOfWeek --------------------------------------------------------------

(def ^:private days (mapv #(impl/value :jolt.time/day-of-week {:value %}) (range 1 8)))
(defn dow [n]
  (or (get days (dec n))
      (throw (jolt.host/throwable "java.time.DateTimeException"
                                  (str "Invalid value for DayOfWeek: " n)))))
(defn dow-val [e] (impl/field e :value))
(defn- dow-name [e] (nth u/day-names (dec (dow-val e))))

(impl/register-type! :jolt.time/day-of-week
  {:eq   (fn [a b] (= (dow-val a) (dow-val b)))
   :hash (fn [e] (dow-val e))
   :str  dow-name
   :enum? true
   :cmp  (fn [a b] (- (dow-val a) (dow-val b)))
   :classes #{"java.time.DayOfWeek" "DayOfWeek"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})

(__register-class-methods! :jolt.time/day-of-week
  {"getValue"  (fn [e] (dow-val e))
   "ordinal"   (fn [e] (dec (dow-val e)))
   "name"      dow-name
   "toString"  dow-name
   "getDisplayName" (fn [e style l] (text-name :days (dec (dow-val e)) style l))
   "plus"      (fn [e n] (dow (inc (u/floor-mod (+ (dec (dow-val e)) (u/->long n)) 7))))
   "minus"     (fn [e n] (dow (inc (u/floor-mod (- (dec (dow-val e)) (u/->long n)) 7))))
   "compareTo" compare
   "equals"    (fn [e o] (boolean (and (impl/jt? o) (= :jolt.time/day-of-week (impl/type-of o))
                                        (= (dow-val e) (dow-val o)))))
   "hashCode"  (fn [e] (dow-val e))})

(enum! "java.time.DayOfWeek" u/day-names days)
(statics! ["DayOfWeek" "java.time.DayOfWeek"] {"of" (fn [n] (dow (u/->long n)))})

;; ---- ChronoUnit -------------------------------------------------------------

(def ^:private units
  (into {} (map (fn [nm] [nm (impl/value :jolt.time/chrono-unit {:name nm})]) u/chrono-units)))
(defn chrono-unit [name]
  (or (get units name)
      (throw (jolt.host/throwable "java.lang.IllegalArgumentException"
                                  (str "No enum constant java.time.temporal.ChronoUnit." name)))))
(defn cu-name [u] (impl/field u :name))

(impl/register-type! :jolt.time/chrono-unit
  {:eq   (fn [a b] (= (cu-name a) (cu-name b)))
   :hash (fn [u] (hash (cu-name u)))
   :str  (fn [u] (display-name (cu-name u)))
   :enum? true
   :cmp  (fn [a b] (- (u/chrono-unit-index (cu-name a))
                      (u/chrono-unit-index (cu-name b))))
   :classes #{"java.time.temporal.ChronoUnit" "ChronoUnit"
              "java.time.temporal.TemporalUnit" "TemporalUnit"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})

(def ^:private time-based-units
  #{"NANOS" "MICROS" "MILLIS" "SECONDS" "MINUTES" "HOURS" "HALF_DAYS"})

(__register-class-methods! :jolt.time/chrono-unit
  {"name"       cu-name
   "toString"   (fn [u] (display-name (cu-name u)))
   "ordinal"    (fn [u] (u/chrono-unit-index (cu-name u)))
   "compareTo"  compare
   "isTimeBased" (fn [u] (contains? time-based-units (cu-name u)))
   "isDateBased" (fn [u] (contains? #{"DAYS" "WEEKS" "MONTHS" "YEARS" "DECADES"
                                      "CENTURIES" "MILLENNIA" "ERAS"} (cu-name u)))
   "isDurationEstimated" (fn [u] (not (contains? time-based-units (cu-name u))))
   "equals"     (fn [u o] (boolean (and (impl/jt? o) (= :jolt.time/chrono-unit (impl/type-of o))
                                         (= (cu-name u) (cu-name o)))))
   "hashCode"   (fn [u] (hash (cu-name u)))})

(enum! "java.time.temporal.ChronoUnit" u/chrono-units (map units u/chrono-units))

;; ---- ChronoField ------------------------------------------------------------

(def ^:private fields
  (into {} (map (fn [nm] [nm (impl/value :jolt.time/chrono-field {:name nm})]) u/chrono-fields)))
(defn chrono-field [name]
  (or (get fields name)
      (throw (jolt.host/throwable "java.lang.IllegalArgumentException"
                                  (str "No enum constant java.time.temporal.ChronoField." name)))))
(defn cf-name [f] (impl/field f :name))
;; the JDK's toString: the camel-case name, AM/PM spelled AmPm (AmPmOfDay)
(defn- cf-display [f] (.replace (display-name (cf-name f)) "Ampm" "AmPm"))

(impl/register-type! :jolt.time/chrono-field
  {:eq   (fn [a b] (= (cf-name a) (cf-name b)))
   :hash (fn [f] (hash (cf-name f)))
   :str  cf-display
   :enum? true
   :cmp  (fn [a b] (- (u/chrono-field-index (cf-name a))
                      (u/chrono-field-index (cf-name b))))
   :classes #{"java.time.temporal.ChronoField" "ChronoField"
              "java.time.temporal.TemporalField" "TemporalField"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})

(__register-class-methods! :jolt.time/chrono-field
  {"name"      cf-name
   "toString"  cf-display
   "ordinal"   (fn [f] (u/chrono-field-index (cf-name f)))
   "compareTo" compare
   ;; CLDR names some fields per locale (DAY_OF_WEEK, MONTH_OF_YEAR, …); the
   ;; rest are their toString in every locale
   "getDisplayName" (fn [f l] (get (:names (jolt.host/extension-value :field-names (locale-id l)))
                                   (cf-name f) (cf-display f)))
   "isDateBased" (fn [f] (contains? u/date-based-fields (cf-name f)))
   "isTimeBased" (fn [f] (not (or (contains? u/date-based-fields (cf-name f))
                                  (contains? #{"INSTANT_SECONDS" "OFFSET_SECONDS"} (cf-name f)))))
   "equals"    (fn [f o] (boolean (and (impl/jt? o) (= :jolt.time/chrono-field (impl/type-of o))
                                        (= (cf-name f) (cf-name o)))))
   "hashCode"  (fn [f] (hash (cf-name f)))})

(enum! "java.time.temporal.ChronoField" u/chrono-fields (map fields u/chrono-fields))

;; ---- ValueRange -------------------------------------------------------------

(defn value-range [smin lmin smax lmax]
  (impl/value :jolt.time/value-range {:smin smin :lmin lmin :smax smax :lmax lmax}))

(impl/register-type! :jolt.time/value-range
  {:eq   (fn [a b] (and (= (impl/field a :lmin) (impl/field b :lmin))
                        (= (impl/field a :lmax) (impl/field b :lmax))))
   :hash (fn [r] (hash [(impl/field r :lmin) (impl/field r :lmax)]))
   :str  (fn [r] (str (impl/field r :lmin) " - " (impl/field r :lmax)))
   :cmp  nil
   :classes #{"java.time.temporal.ValueRange" "ValueRange" "java.io.Serializable" "Serializable"}})

(__register-class-methods! :jolt.time/value-range
  {"getMinimum" (fn [r] (impl/field r :lmin))
   "getLargestMinimum" (fn [r] (impl/field r :lmin))
   "getSmallestMaximum" (fn [r] (impl/field r :smax))
   "getMaximum" (fn [r] (impl/field r :lmax))
   "isFixed" (fn [r] (and (= (impl/field r :smin) (impl/field r :lmin))
                          (= (impl/field r :smax) (impl/field r :lmax))))
   "isValidValue" (fn [r v] (let [x (u/->long v)] (and (>= x (impl/field r :lmin)) (<= x (impl/field r :lmax)))))
   "toString" (fn [r] (str (impl/field r :lmin) " - " (impl/field r :lmax)))})

(statics! ["ValueRange" "java.time.temporal.ValueRange"]
  {"of" (fn ([mn mx] (value-range (u/->long mn) (u/->long mn) (u/->long mx) (u/->long mx)))
            ([mn smx lmx] (value-range (u/->long mn) (u/->long mn) (u/->long smx) (u/->long lmx)))
            ([smn lmn smx lmx] (value-range (u/->long smn) (u/->long lmn) (u/->long smx) (u/->long lmx))))})
