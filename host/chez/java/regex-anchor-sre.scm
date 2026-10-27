;; regex-anchor-sre.scm — O(1) look-behind for single-character anchors (jolt #1062).
;;
;; Adapted from irregex's sre->procedure, Copyright (c) 2005-2024 Alex Shinn,
;; BSD-style license (licenses/BSD-irregex.txt).
;;
;; `sre->procedure` below is a verbatim copy of the vendored IrRegex definition
;; (vendor/irregex/irregex.scm), redefined globally so it wins over the vendored
;; one at load time.  The changes are marked "jolt #1062" and are these: the
;; `look-behind` arm gains an O(1) fast path for single-character bodies and a
;; width-bounded rescan for wider ones; the symbol arm gains the zero-width
;; `%look-behind-end` assertion that bounded rescan needs, and a direct compilation
;; of the java.util.regex line anchors (host/chez/java/regex-anchors.ss).  The
;; repetition arms (`*`, `*?`, `+`, `**`, `**?`) are marked "jolt-0e5k": a body
;; that can match empty no longer raises "invalid sre: empty *" or loops forever,
;; it runs under java.util.regex's zero-length-iteration rule (see %iterate below).
;; Those arms also memoize failed loop decisions, as the JVM's Loop does, so
;; nested quantifiers stop taking exponential time (jolt-yqr, %loop-memo-plan),
;; and compile a counted body once rather than on every iteration. The file
;; ends with a copy of irregex's `cset-union` whose merge keeps a char set
;; sorted (jolt-t16).
;;
;; Why: the vendored `look-behind` compiles its body as `(* any) X eos` against a
;; chunk wrapped to end at the current position, so it rescans from the chunk
;; start to that position on every evaluation.  A one-character look-behind only
;; ever inspects the immediately preceding code unit, yet pays O(n) per position
;; and O(n^2) for a scan.  The java.util.regex `^`/`$`/`\A`/`\Z` anchors are built
;; from such look-behinds (host/chez/java/regex-translate.ss), so every anchor —
;; and every pattern containing one — was quadratic.
;;
;; Keep in sync with upstream by hand, exactly as host/chez/regex-dfa.ss redefines
;; the vendored `nfa->dfa`; host/chez/regex-dfa-check.sh is the same kind of hook.

;; The char set of a look-behind body when that body is a single character or a
;; char-set operator (`or`, `~`, `/`, `&`, `-`), else #f so the general path runs.
;; `flags` is threaded through so a case-insensitive match folds the body exactly
;; as the general path does.  `sre->cset` also accepts strings, but a multi-char
;; string is a sequence, not a single unit, so only the shapes above are admitted.
(define (%prev-char-cset sre flags)
  ;; A body is a one-unit look-behind only when it is a char, or a char-set
  ;; operator (~ & - / or) whose leaves are ALL chars.  A string leaf is
  ;; rejected outright: `sre->cset` maps a string to the set of its characters
  ;; (`(rec (list sre))` -> `string->cset`), so `(or "ab" "cd")` would wrongly
  ;; read as the single-unit set {a,b,c,d} and the fast path would misfire.
  (define (chars-only? x)
    (cond ((char? x) #t)
          ((and (pair? x) (memq (car x) '(~ & - / or)))
           (let loop ((xs (cdr x)))
             (or (null? xs) (and (chars-only? (car xs)) (loop (cdr xs))))))
          (else #f)))
  (and (chars-only? sre)
       (guard (e (#t #f)) (sre->cset sre (flag-set? flags ~case-insensitive?)))))

;; Set by the bounded look-behind rescan to the width it clamped to, so the
;; clock-free test (test/chez/regex-anchor-test.ss) can witness that the rescan
;; window is the body's own width and never grows with the input.  Never read by
;; matching code.
(define %look-behind-window 0)

;; jolt #1062 / drg-bba2: the target end position for the innermost look-behind
;; currently evaluating its body.  Bound around the body's evaluation so the
;; zero-width `%look-behind-end` assertion can pin the body's end back to i even
;; when the wrapped chunk was widened past i to let a nested look-around read.
(define %look-behind-target 0)

;; An UPPER BOUND, in code units, on how many units an SRE body can consume — or
;; #f when the body is unbounded or its width is not statically known.  A
;; look-behind body that spans at most K units cannot have begun more than K units
;; behind the current position, so the rescan below only has to start at (i - K)
;; instead of walking back to the chunk start; that walk was #1062's O(n) per
;; evaluation.
;;
;; The result is always a valid over-approximation (or #f, which disables the
;; bound), so it can never hide a match: `or` takes its widest branch, a bounded
;; quantifier its widest expansion, and any shape not modelled answers #f, leaving
;; the general rescan in place.
(define (%sre-max-length sre)
  (define (seq-max xs)
    (let loop ((xs xs) (tot 0))
      (cond ((null? xs) tot)
            (else (let ((m (%sre-max-length (car xs))))
                    (and m (loop (cdr xs) (+ tot m))))))))
  (cond
    ((char? sre) 1)
    ((string? sre) (string-length sre))
    ((symbol? sre)
     (case sre
       ((any nonl) 1)
       ((bos bol bow eos eol eow nwb epsilon) 0)
       ;; jolt #1062: the java.util.regex line-anchor primitives
       ;; (regex-anchors.ss) are zero-width assertions, exactly like `eos`
       ;; and `eol` above, and are bounded the same way.
       ((%java-bol %java-eol %java-final-eol
         %java-bol-unix %java-final-eol-unix) 0)
       (else #f)))
    ((pair? sre)
     (case (car sre)
       ((seq : atomic w/case w/nocase w/utf8 w/noutf8 word) (seq-max (cdr sre)))
       ((or) (let loop ((xs (cdr sre)) (best 0))
               (if (null? xs)
                   best
                   (let ((m (%sre-max-length (car xs))))
                     (and m (loop (cdr xs) (max best m)))))))
       ((~ & - / posix-string) 1)
       ((? ??) (%sre-max-length (cadr sre)))
       ((* *? + +? word+) #f)
       ((** **?) (let ((hi (caddr sre)))
                   (and (number? hi)
                        (let ((w (seq-max (cdddr sre))))
                          (and w (* hi w))))))
       ((=) (and (number? (cadr sre))
                 (let ((w (seq-max (cddr sre))))
                   (and w (* (cadr sre) w)))))
       ((>=) #f)
       ((look-ahead neg-look-ahead look-behind neg-look-behind) 0)
       (else #f)))
    (else #f)))

;; Forward reach of an SRE matched FORWARD from the current position: how far past
;; its start a match may read.  A look-around contributes the reach of its own
;; body.  #f means unbounded (or not modelled).  Bounds a nested look-ahead's body
;; so a look-behind can size the chunk it hands to that look-ahead (drg-bba2).
(define (%sre-forward-span sre)
  (define (seq-span xs)
    (let loop ((xs xs) (tot 0))
      (cond ((null? xs) tot)
            (else (let ((m (%sre-forward-span (car xs))))
                    (and m (loop (cdr xs) (+ tot m))))))))
  (cond
    ((char? sre) 1)
    ((string? sre) (string-length sre))
    ((symbol? sre)
     (case sre
       ((any nonl) 1)
       ((bos bol bow eos eol eow nwb epsilon) 0)
       ;; jolt #1062: the java.util.regex line-anchor primitives
       ;; (regex-anchors.ss) are zero-width assertions, exactly like `eos`
       ;; and `eol` above, and are bounded the same way.
       ((%java-bol %java-eol %java-final-eol
         %java-bol-unix %java-final-eol-unix) 0)
       (else #f)))
    ((pair? sre)
     (case (car sre)
       ((seq : atomic w/case w/nocase w/utf8 w/noutf8 word) (seq-span (cdr sre)))
       ((or) (let loop ((xs (cdr sre)) (best 0))
               (if (null? xs)
                   best
                   (let ((m (%sre-forward-span (car xs))))
                     (and m (loop (cdr xs) (max best m)))))))
       ((~ & - / posix-string) 1)
       ((? ??) (seq-span (cdr sre)))
       ((* *? + +? word+) #f)
       ((** **?) (let ((hi (caddr sre)))
                   (and (number? hi)
                        (let ((w (seq-span (cdddr sre))))
                          (and w (* hi w))))))
       ((=) (and (number? (cadr sre))
                 (let ((w (seq-span (cddr sre))))
                   (and w (* (cadr sre) w)))))
       ((>=) #f)
       ((look-ahead neg-look-ahead look-behind neg-look-behind)
        (%sre-forward-span (cadr sre)))
       (else #f)))
    (else #f)))

;; How far past i a forward-reading assertion in a look-behind body must be able
;; to see; see the symbol arm of %sre-lookbehind-ext below for why three.
(define %lookbehind-assertion-ext 3)

;; The extra forward reach a LOOK-BEHIND body needs PAST the position i at which it
;; must end: its consuming parts all end at or before i, so only a nested
;; look-AROUND reads past i.  This is the width to extend the wrapped chunk by so an
;; inner look-ahead can see the unit the outer tail matches; an explicit
;; `(= ext any)` re-anchors the body's end back to i (drg-bba2).  Always a valid
;; over-approximation -- or #f, which restores the plain full-chunk wrap.
(define (%sre-lookbehind-ext sre)
  (define (max-ext xs)
    (let loop ((xs xs) (best 0))
      (cond ((null? xs) best)
            (else (let ((m (%sre-lookbehind-ext (car xs))))
                    (and m (loop (cdr xs) (max best m))))))))
  (cond
    ((pair? sre)
     (case (car sre)
       ((=) (max-ext (cddr sre)))
       ((** **?) (max-ext (cdddr sre)))
       ((>=) #f)
       ((~ & - / posix-string) 0)
       ((look-ahead neg-look-ahead look-behind neg-look-behind)
        (%sre-forward-span (cadr sre)))
       ((seq : atomic w/case w/nocase w/utf8 w/noutf8 word or
         ? ?? * *? + +? word+) (max-ext (cdr sre)))
       (else #f)))
    ;; jolt-69q: an assertion that READS FORWARD from the position it is tested
    ;; at cannot be evaluated against a chunk wrapped to end at i — the wrap IS
    ;; the end of input as far as it can tell, so it answers about the wrap
    ;; instead of about the subject.  Whether i ends the input, ends a line, or
    ;; ends a word are all questions about what comes AFTER i:
    ;;
    ;;   (re-find #"(?<=a$)" "ab")   matched at 1, where the JVM does not
    ;;   (re-find #"(?<=a\b)" "ab")  likewise — eow saw the wrap, not the 'b'
    ;;   (re-find #"(?m)(?<=^)" "ab") found NOTHING, where the JVM finds 0:
    ;;     %java-bol is "...and not at the end of input", so the wrap made
    ;;     every position look like the end and the anchor declined them all
    ;;
    ;; A FINITE widening is enough, and #f (the whole chunk) is not needed: each of
    ;; these assertions only has to see a bounded distance past i to answer.  The
    ;; chunk is wrapped to min(i + ext, real end), so when the real input runs out
    ;; first the assertion is reading the true end, and when it does not the window
    ;; stays full — which is itself the answer, because a full window means more
    ;; input follows.  %look-behind-end then pins the body's own end back to i, so
    ;; the widening lets the assertion LOOK past i without letting the body MATCH
    ;; past it.
    ;;
    ;; Three is the widest any of them needs:
    ;;   \z, eos          1 — is there any unit after i at all
    ;;   bow, eow, nwb     1 — the character AT i (bow reads forward, despite the name)
    ;;   %java-bol         1 — Java's ^ is "...and not at the end of input"
    ;;   $, \Z            3 — "end of input, or before the FINAL line terminator".
    ;;                         The longest terminator is \r\n, two units, so a
    ;;                         still-full three-unit window cannot be a lone
    ;;                         terminator and the assertion correctly declines.
    ;;
    ;; Keeping it finite is the point: ext = #f would be correct too, but it hands
    ;; the body the whole chunk, and the (* any) prefix then runs to the end of the
    ;; input at every position — O(n^2) for a scan, which is exactly the cost #1062
    ;; exists to remove.  Measured over a 10k-unit subject: (?<=a$) scans in 1.8 ms
    ;; with the bounded window against 1582 ms with the whole chunk, and (?<=\b) in
    ;; 2.8 ms against 2814 ms.
    ;;
    ;; bos is the one assertion left at 0: \A is i = 0 and nothing else, a purely
    ;; backward question no wrap can affect.
    ((symbol? sre)
     (case sre
       ((eos eol eow nwb bow bol
         %java-bol %java-eol %java-final-eol
         %java-bol-unix %java-final-eol-unix) %lookbehind-assertion-ext)
       (else 0)))
    (else 0)))

;; The "word character" a word BOUNDARY is a boundary between.  java.util.regex
;; defines \b in terms of \w, which is [a-zA-Z0-9_], and regex-translate.ss
;; already spells \w as (or alphanumeric #\_) for exactly that reason.  irregex's
;; char-alphanumeric? leaves the underscore out -- its own (word ...) SRE adds it
;; back by hand, which is the tell -- so bow/eow/nwb disagreed with the \w sitting
;; beside them in the same pattern: (re-seq #"\b" "a_b") was (0 1 2 3), splitting
;; the identifier at its underscore, against the JVM's (0 3).  jolt-406.
(define (%word-char? c) (or (char-alphanumeric? c) (eqv? c #\_)))

;; jolt-0e5k: a repetition over a body that can match empty — (?:c?)*, (x?)*y,
;; (?:.*?|a)+ — is legal Java and common, and irregex either refused it ("invalid
;; sre: empty *") or, where its `sre-empty?` missed the body, compiled a loop that
;; re-entered the body at the same position forever.  java.util.regex's Loop and
;; LazyLoop run such a body under one rule: an iteration that consumed nothing ENDS
;; the repetition, and the match carries on with what follows it.
;;
;; That needs the position the current iteration started at, and the body's
;; continuation is compiled once, so it is kept in a per-repetition cell: %iterate
;; records (src . i) before entering the body and puts the previous value back
;; when the body fails, so a backtrack into an earlier iteration finds that
;; iteration's start again.  Only a body %sre-nullable? says can be empty pays
;; any of this; every other repetition compiles exactly as irregex wrote it.
;;
;; Per-attempt state lives in the match vector, never in anything shared. A
;; compiled pattern is cached and run by many threads at once, and a fiber can be
;; preempted mid-match and resume after another fiber has matched on the same
;; carrier, or on another carrier: a thread parameter is shared by the threads
;; forked after it was set (they inherit the same object) and by the fibers of
;; one carrier. The match vector is made for each search by its caller and
;; reaches every step as `matches`. irregex sizes it for one group more than the
;; pattern has and uses only that group's last slot (the backtrack fail), so the
;; three before it are free: one holds the memo state below, one the cells.
;; irregex clears them with the groups between attempts.
(define (%memo-slot matches) (fx- (vector-length matches) 4))
(define (%cells-slot matches) (fx- (vector-length matches) 3))

;; A cell is an index into the attempt's cell vector, numbered when the pattern
;; is compiled (%new-cell!); the vector grows to take a cell compiled later (a
;; counted body is compiled on first use).
(define (%cell-ref matches k)
  (let ((v (vector-ref matches (%cells-slot matches))))
    (and (vector? v) (fx< k (vector-length v)) (vector-ref v k))))
(define (%cell-set! matches k x)
  (let* ((slot (%cells-slot matches))
         (v (vector-ref matches slot)))
    (if (and (vector? v) (fx< k (vector-length v)))
        (vector-set! v k x)
        (let ((w (make-vector (fxmax (fx+ k 1) (fx* 2 (if (vector? v) (vector-length v) 4))) #f)))
          (when (vector? v)
            (do ((j 0 (fx+ j 1))) ((fx= j (vector-length v))) (vector-set! w j (vector-ref v j))))
          (vector-set! w k x)
          (vector-set! matches slot w)))))

(define %cell-counter 0)
(define-process-local %cell-mutex (make-mutex))
(define (%new-cell!)
  (with-mutex %cell-mutex
    (let ((k %cell-counter)) (set! %cell-counter (fx+ k 1)) k)))

(define (%iterate cell body cnk init src str i end matches fail)
  (let ((old (%cell-ref matches cell)))
    (%cell-set! matches cell (cons src i))
    (body cnk init src str i end matches
          (lambda () (%cell-set! matches cell old) (fail)))))

;; Did the iteration whose body just matched up to (src, i) consume nothing?
(define (%iteration-empty? cell matches src i)
  (let ((start (%cell-ref matches cell)))
    (and (pair? start) (eq? (car start) src) (eqv? (cdr start) i))))

;; jolt-0e5k: java.util.regex compiles a capturing group whose body always
;; matches exactly nothing — (^), (), (\b), ((?=x)) — and is quantified greedily
;; from zero as a GroupCurly, which runs the body once and, finding it consumed
;; nothing, ends the loop WITHOUT recording the group: (re-find #"(^)*x" "x") is
;; ["x" nil]. Groups inside the body keep what they captured. So `(* (submatch X))`
;; over such an X is `(? X)` with the group's number reserved but never set,
;; which is what `(dsm 1 0 X)` says. This answers X's sequence, or #f.
;;
;; "Always nothing" is Java's deterministic zero length: assertions and empty
;; groups, but no alternation of real branches — ((?=x)|(?=y))* runs as a Loop and
;; does capture. Java's \b is one node, so the (or bow eow) it translates to counts.
(define (%zero-width-group-body sre)
  (define (assertion? x)
    (or (memq x '(bos eos bol eol bow eow nwb epsilon
                  %java-bol %java-eol %java-final-eol
                  %java-bol-unix %java-final-eol-unix))
        (equal? x "")))
  (define (det-empty? x)
    (cond ((assertion? x) #t)
          ((pair? x)
           (case (car x)
             ((look-ahead look-behind neg-look-ahead neg-look-behind) #t)
             ((or) (let lp ((xs (cdr x)))
                     (or (null? xs) (and (assertion? (car xs)) (lp (cdr xs))))))
             ((seq : submatch $) (all? (cdr x)))
             ((=> submatch-named) (all? (cddr x)))
             (else #f)))
          (else #f)))
  (define (all? xs) (or (null? xs) (and (det-empty? (car xs)) (all? (cdr xs)))))
  (and (pair? sre)
       (case (car sre)
         ((submatch $) (and (all? (cdr sre)) (cdr sre)))
         ((=> submatch-named) (and (all? (cddr sre)) (cddr sre)))
         (else #f))))

;; What a repetition's body continues with once an iteration has matched: `more`
;; (the next decision), unless the body can be empty and this iteration was —
;; then the loop ends (jolt-0e5k).
(define (%after-iteration nullable? cell next more)
  (if nullable?
      (lambda (cnk init src str i end matches fail)
        (if (%iteration-empty? cell matches src i)
            (next cnk init src str i end matches fail)
            (more cnk init src str i end matches fail)))
      more))

;; Iteration `count`'s compiled body for a counted repetition, built on first use
;; and kept. irregex built a fresh closure tree for every iteration of every
;; match, which made each pass through a {0,2}? or {2,} cost a compile. Counts
;; past `from` with no upper bound behave alike, so at most from+1 are ever
;; asked for; a bound past the cache's size compiles beyond it on demand, as
;; before. Filling a slot from two threads at once builds two equivalent bodies
;; and keeps either.
(define %iteration-cache-limit 256)
(define (%iteration-bodies from to compile)
  (let ((cache (make-vector (+ 1 (min %iteration-cache-limit (or to (+ from 1)))) #f)))
    (lambda (count)
      (if (< count (vector-length cache))
          (or (vector-ref cache count)
              (let ((b (compile count)))
                (vector-set! cache count b)
                b))
          (compile count)))))

;; jolt-yqr: java.util.regex runs nested quantifiers like
;;   (.{0,2}?c|b+[ab]c*?|(?:(c{0,2}?|ab|ba)*?(a*|b*c*)(c??|aaa|a{1,3}?)+?|.??b.)*a[ab]
;; in linear time, and the backtracker took seconds on eight characters, about 8x
;; for each one more. The JVM's Loop remembers the positions at which another
;; iteration has already failed and goes straight on from there (posIndex,
;; JDK-6328855); without it every way of splitting the input among the inner
;; quantifiers is tried again on every return to the same position.
;;
;; Here the memoized step is a loop's whole decision at position i — one more
;; iteration, else what follows — and what is remembered is that it FAILED, every
;; path through it exhausted. A later arrival at i fails at once. That is sound
;; only where the decision's outcome depends on i and nothing else, so a loop is
;; memoized only when
;;   - its count no longer matters: past its minimum, with no upper bound;
;;   - it is not inside another repetition, whose own count and iteration start
;;     are part of what follows it, nor inside a look-around or atomic group,
;;     whose body ends somewhere that varies per evaluation;
;;   - the pattern has no backreference, which would make captures part of the
;;     outcome. Captures otherwise only decide WHAT a match returns, never
;;     whether one exists, and the memo only prunes subtrees with no match.
;; A loop whose body matches one way only (a char set, a literal) cannot cause
;; the blowup and is left alone, so #"\w*" and #"[^,]*," pay nothing.
;;
;; The memo lives for one match attempt from one start position — %memo-root
;; resets it — and only positions in the starting chunk are recorded. It stays
;; dormant until the attempt has made %memo-threshold loop decisions: an ordinary
;; match never gets there, so it pays a counter and no table, while a search that
;; is going exponential passes it almost at once. Remembering only some failures
;; is as sound as remembering all of them. The state is #(chunk decisions table),
;; made fresh for each attempt in the match vector's memo slot (%memo-slot).
;; Answers #f when no loop qualifies, else
;;   #(loop-node->id  number-of-ids).
(define (%loop-memo-plan sre)
  (define ids (make-eq-hashtable))
  (define count 0)
  (define backref? #f)
  (define (ambiguous? x)
    (and (pair? x)
         (case (car x)
           ((? ?? * *? + +? ** **? = >= backref backref-ci) #t)
           ((or) (not (%sre-cset-able? x)))
           ((=> submatch-named) (exists ambiguous? (cddr x)))
           (else (and (list? x) (exists ambiguous? (cdr x)))))))
  (define (note! x body)
    (when (exists ambiguous? body)
      (hashtable-set! ids x count)
      (set! count (+ count 1))))
  (define (walk* xs top?) (when (list? xs) (for-each (lambda (x) (walk x top?)) xs)))
  (define (walk x top?)
    (when (pair? x)
      (case (car x)
        ((backref backref-ci) (set! backref? #t))
        ((* *? +) (when top? (note! x (cdr x))) (walk* (cdr x) #f))
        ((** **?) (when (and top? (not (caddr x))) (note! x (cdddr x)))
                  (walk* (cdddr x) #f))
        ((+? = >= look-ahead look-behind neg-look-ahead neg-look-behind atomic)
         (walk* (cdr x) #f))
        ((=> submatch-named) (walk* (cddr x) top?))
        ((dsm) (walk* (cdddr x) top?))
        (else (walk* (cdr x) top?)))))
  (walk sre #t)
  (and (not backref?) (> count 0)
       (vector ids count)))

(define %memo-threshold 512)

(define (%memo-wrap plan id decide)
  (let ((width (vector-ref plan 1)))
    (lambda (cnk init src str i end matches fail)
      (let* ((st (vector-ref matches (%memo-slot matches)))
             (n (fx+ (vector-ref st 1) 1)))
        (vector-set! st 1 n)
        (if (or (fx< n %memo-threshold) (not (eq? src (vector-ref st 0))))
            (decide cnk init src str i end matches fail)
            (let ((key (+ (* i width) id))
                  (t (vector-ref st 2)))
              (if (and t (hashtable-ref t key #f))
                  (fail)
                  (decide cnk init src str i end matches
                          (lambda ()
                            (let ((t (or (vector-ref st 2)
                                         (let ((t (make-eqv-hashtable)))
                                           (vector-set! st 2 t)
                                           t))))
                              (hashtable-set! t key #t))
                            (fail))))))))))

;; The compiled pattern, behind a fresh memo for each attempt.
(define (%memo-root plan root)
  (lambda (cnk init src str i end matches fail)
    (vector-set! matches (%memo-slot matches) (vector src 0 #f))
    (root cnk init src str i end matches fail)))

(define (sre->procedure sre . o)
  (define names
    (if (and (pair? o) (pair? (cdr o))) (cadr o) (sre-names sre 1 '())))
  ;; jolt-yqr: which loops may remember failed positions, and the wrapper that
  ;; does it for one of them (see %loop-memo-plan).
  (define plan (%loop-memo-plan sre))
  (define (memoize node decide)
    (let ((id (and plan (hashtable-ref (vector-ref plan 0) node #f))))
      (if id (%memo-wrap plan id decide) decide)))
  (define root
  (let lp ((sre sre)
           (n 1)
           (flags (if (pair? o) (car o) ~none))
           (next (lambda (cnk init src str i end matches fail)
                   (irregex-match-start-chunk-set! matches 0 (car init))
                   (irregex-match-start-index-set! matches 0 (cdr init))
                   (irregex-match-end-chunk-set! matches 0 src)
                   (irregex-match-end-index-set! matches 0 i)
                   (%irregex-match-fail-set! matches fail)
                   matches)))
    ;; XXXX this should be inlined
    (define (rec sre) (lp sre n flags next))
    (cond
     ((pair? sre)
      (if (string? (car sre))
          (sre-cset->procedure
           (sre->cset (car sre) (flag-set? flags ~case-insensitive?))
           next)
          (case (car sre)
            ((~ - & /)
             (sre-cset->procedure
              (sre->cset sre (flag-set? flags ~case-insensitive?))
              next))
            ((or)
             ;; jolt #1062: an alternation of single code units is a char SET, and
             ;; one binary search beats a chain of alternation closures — which is
             ;; how every java.util.regex character class arrives here (`[a-z_]`,
             ;; `\w`, `\s` are all `or`).  regex-anchors.ss says when the rewrite
             ;; is legal; when it is not, the vendored alternation below runs.
             (if (%sre-cset-able? sre)
                 (sre-cset->procedure
                  (sre->cset sre (flag-set? flags ~case-insensitive?))
                  next)
             (case (length (cdr sre))
               ((0) (lambda (cnk init src str i end matches fail) (fail)))
               ((1) (rec (cadr sre)))
               (else
                (let* ((first (rec (cadr sre)))
                       (rest (lp (sre-alternate (cddr sre))
                                 (+ n (sre-count-submatches (cadr sre)))
                                 flags
                                 next)))
                  (lambda (cnk init src str i end matches fail)
                    (first cnk init src str i end matches
                           (lambda ()
                             (rest cnk init src str i end matches fail)))))))))
            ((w/case)
             (lp (sre-sequence (cdr sre))
                 n
                 (flag-clear flags ~case-insensitive?)
                 next))
            ((w/nocase)
             (lp (sre-sequence (cdr sre))
                 n
                 (flag-join flags ~case-insensitive?)
                 next))
            ((w/utf8)
             (lp (sre-sequence (cdr sre)) n (flag-join flags ~utf8?) next))
            ((w/noutf8)
             (lp (sre-sequence (cdr sre)) n (flag-clear flags ~utf8?) next))
            ((seq :)
             (case (length (cdr sre))
               ((0) next)
               ((1) (rec (cadr sre)))
               (else
                (let ((rest (lp (sre-sequence (cddr sre))
                                (+ n (sre-count-submatches (cadr sre)))
                                flags
                                next)))
                  (lp (cadr sre) n flags rest)))))
            ((?)
             (let ((body (rec (sre-sequence (cdr sre)))))
               (lambda (cnk init src str i end matches fail)
                 (body cnk init src str i end matches
                       (lambda () (next cnk init src str i end matches fail))))))
            ((??)
             (let ((body (rec (sre-sequence (cdr sre)))))
               (lambda (cnk init src str i end matches fail)
                 (next cnk init src str i end matches
                       (lambda () (body cnk init src str i end matches fail))))))
            ((*)
             (cond
              ((%zero-width-group-body (sre-sequence (cdr sre)))
               => (lambda (body) (rec `(? (dsm 1 0 ,@body)))))
              (else
               ;; `more` is the decision at i: one more iteration, else what
               ;; follows. jolt-0e5k: a nullable body runs each iteration through
               ;; %iterate, and an empty one ends the loop. jolt-yqr: `more` is
               ;; memoized when %loop-memo-plan says it may be.
               (let* ((x (sre-sequence (cdr sre)))
                      (nullable? (%sre-nullable? x))
                      (cell (%new-cell!)))
                 (letrec*
                     ((more
                       (memoize sre
                        (lambda (cnk init src str i end matches fail)
                          (let ((retreat (lambda () (next cnk init src str i end matches fail))))
                            (if nullable?
                                (%iterate cell body cnk init src str i end matches retreat)
                                (body cnk init src str i end matches retreat))))))
                      (body (lp x n flags (%after-iteration nullable? cell next more))))
                   more)))))
            ((*?)
             (let* ((x (sre-sequence (cdr sre)))
                    (nullable? (%sre-nullable? x))
                    (cell (%new-cell!)))
               ;; lazy: what follows first, then one more iteration.
               (letrec*
                   ((try
                     (memoize sre
                      (lambda (cnk init src str i end matches fail)
                        (next cnk init src str i end matches
                              (lambda ()
                                (if nullable?
                                    (%iterate cell body cnk init src str i end matches fail)
                                    (body cnk init src str i end matches fail)))))))
                    (body (lp x n flags (%after-iteration nullable? cell next try))))
                 try)))
            ((+)
             (let* ((x (sre-sequence (cdr sre)))
                    (nullable? (%sre-nullable? x))
                    (cell (%new-cell!)))
               ;; the first iteration is required; after it, `more` as for `*`.
               (letrec*
                   ((more
                     (memoize sre
                      (lambda (cnk init src str i end matches fail)
                        (let ((retreat (lambda () (next cnk init src str i end matches fail))))
                          (if nullable?
                              (%iterate cell body cnk init src str i end matches retreat)
                              (body cnk init src str i end matches retreat))))))
                    (body (lp x n flags (%after-iteration nullable? cell next more))))
                 (if nullable?
                     (lambda (cnk init src str i end matches fail)
                       (%iterate cell body cnk init src str i end matches fail))
                     body))))
            ((=)
             (rec `(** ,(cadr sre) ,(cadr sre) ,@(cddr sre))))
            ((>=)
             (rec `(** ,(cadr sre) #f ,@(cddr sre))))
            ((**)
             (cond
              ((and (eqv? (cadr sre) 0)
                    (not (eqv? (caddr sre) 0))
                    (%zero-width-group-body (sre-sequence (cdddr sre))))
               ;; jolt-0e5k: {0,n} over a zero-width group, as `*` above.
               => (lambda (body) (rec `(? (dsm 1 0 ,@body)))))
              ((or (and (number? (cadr sre))
                        (number? (caddr sre))
                        (> (cadr sre) (caddr sre)))
                   (and (not (cadr sre)) (caddr sre)))
               (lambda (cnk init src str i end matches fail) (fail)))
              ((and (eqv? (cadr sre) 0) (eqv? (caddr sre) 0))
               next)
              (else
               (let* ((from (cadr sre))
                      (to (caddr sre))
                      (x (sre-sequence (cdddr sre)))
                      (nullable? (%sre-nullable? x))
                      (cell (%new-cell!)))
                 (define (enter b cnk init src str i end matches fail)
                   (if nullable?
                       (%iterate cell b cnk init src str i end matches fail)
                       (b cnk init src str i end matches fail)))
                 (letrec*
                     ;; iteration `count` (1-based). Past `from` with no upper
                     ;; bound every count behaves alike, so all of them share
                     ;; one body and one memoizable `more`.
                     ((body
                       (%iteration-bodies from to
                        (lambda (count)
                          (lp x n flags
                              (lambda (cnk init src str i end matches fail)
                                (cond
                                 ((or (and to (= count to))
                                      (and nullable? (%iteration-empty? cell matches src i)))
                                  (next cnk init src str i end matches fail))
                                 ((and (not to) (>= count from))
                                  (more cnk init src str i end matches fail))
                                 (else
                                  (enter (body (+ 1 count))
                                         cnk init src str i end matches
                                         (lambda ()
                                           (if (>= count from)
                                               (next cnk init src str i end matches fail)
                                               (fail)))))))))))
                      (more
                       (memoize sre
                        (lambda (cnk init src str i end matches fail)
                          (enter (body (+ 1 from)) cnk init src str i end matches
                                 (lambda () (next cnk init src str i end matches fail)))))))
                   (if (and (zero? from) (not to))
                       more
                       (lambda (cnk init src str i end matches fail)
                         (enter (body 1) cnk init src str i end matches
                                (lambda ()
                                  (if (zero? from)
                                      (next cnk init src str i end matches fail)
                                      (fail)))))))))))
            ((**?)
             (cond
              ((or (and (number? (cadr sre))
                        (number? (caddr sre))
                        (> (cadr sre) (caddr sre)))
                   (and (not (cadr sre)) (caddr sre)))
               (lambda (cnk init src str i end matches fail) (fail)))
              ((and (eqv? (cadr sre) 0) (eqv? (caddr sre) 0))
               next)
              (else
               (let* ((from (cadr sre))
                      (to (caddr sre))
                      (x (sre-sequence (cdddr sre)))
                      (nullable? (%sre-nullable? x))
                      (cell (%new-cell!)))
                 (define (enter b cnk init src str i end matches fail)
                   (if nullable?
                       (%iterate cell b cnk init src str i end matches fail)
                       (b cnk init src str i end matches fail)))
                 (letrec*
                     ((body
                       (%iteration-bodies from to
                        (lambda (count)
                          (lp x n flags
                              (lambda (cnk init src str i end matches fail)
                                (cond
                                 ((and nullable? (%iteration-empty? cell matches src i))
                                  (next cnk init src str i end matches fail))
                                 ((< count from)
                                  (enter (body (+ 1 count)) cnk init
                                         src str i end matches fail))
                                 ((not to)
                                  (try cnk init src str i end matches fail))
                                 (else
                                  (next cnk init src str i end matches
                                        (lambda ()
                                          (if (= count to)
                                              (fail)
                                              (enter (body (+ 1 count)) cnk init
                                                     src str i end matches fail)))))))))))
                      ;; lazy, past `from` with no upper bound: what follows
                      ;; first, then one more iteration.
                      (try
                       (memoize sre
                        (lambda (cnk init src str i end matches fail)
                          (next cnk init src str i end matches
                                (lambda ()
                                  (enter (body (+ 1 from)) cnk init src str i end
                                         matches fail)))))))
                   (cond
                    ((and (zero? from) (not to)) try)
                    ((zero? from)
                     (lambda (cnk init src str i end matches fail)
                       (next cnk init src str i end matches
                             (lambda ()
                               (enter (body 1) cnk init src str i end matches fail)))))
                    (else
                     (lambda (cnk init src str i end matches fail)
                       (enter (body 1) cnk init src str i end matches fail)))))))))
            ((word)
             (rec `(seq bow ,@(cdr sre) eow)))
            ((word+)
             (rec `(seq bow (+ (& (or alphanumeric "_")
                                  (or ,@(cdr sre)))) eow)))
            ((posix-string)
             (rec (string->sre (cadr sre))))
            ((look-ahead)
             (let ((check
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (cnk init src str i end matches fail) i))))
               (lambda (cnk init src str i end matches fail)
                 (if (check cnk init src str i end matches (lambda () #f))
                     (next cnk init src str i end matches fail)
                     (fail)))))
            ((neg-look-ahead)
             (let ((check
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (cnk init src str i end matches fail) i))))
               (lambda (cnk init src str i end matches fail)
                 (if (check cnk init src str i end matches (lambda () #f))
                     (fail)
                     (next cnk init src str i end matches fail)))))
             ((look-behind neg-look-behind)
              ;; jolt #1062: a look-behind only inspects the units just before
              ;; the current position.  A single-unit body is tested directly
              ;; (O(1)); a body of bounded width K begins its rescan at (i - K)
              ;; instead of re-running (`(* any)` X eos) from the chunk start
              ;; (O(position)).  A body of unknown width keeps the general path.
              (cond
                ((and (pair? (cdr sre)) (null? (cddr sre)) (%prev-char-cset (cadr sre) flags))
                 (let ((pos? (eq? (car sre) 'look-behind))
                       (cs (%prev-char-cset (cadr sre) flags)))
                   (lambda (cnk init src str i end matches fail)
                     (let ((ch (if (> i ((chunker-get-start cnk) src))
                                   (string-ref str (- i 1))
                                   (chunker-prev-char cnk init src))))
                       (if (eq? pos? (and ch (cset-contains? cs ch)))
                           (next cnk init src str i end matches fail)
                           (fail))))))
                (else
                  ;; jolt #1062 / drg-bba2: a look-behind only inspects the units
                  ;; just before the current position.  Compile the body to end AT
                  ;; i via `(* any) BODY <end>`, and rescan back no farther than
                  ;; the body's width K.  When the body nests a look-around it may
                  ;; legitimately read PAST i, so widen the wrapped chunk to
                  ;; i + EXT and terminate the body with the zero-width
                  ;; `%look-behind-end` assertion instead of `eos`: it pins the
                  ;; body's end back to i without consuming, so it never demands
                  ;; characters that need not exist.  EXT is #f when the inner
                  ;; reach is unbounded, in which case the body gets the whole
                  ;; chunk (correct, but O(position) to rescan).
                  (let* ((ext (%sre-lookbehind-ext (cadr sre)))
                         (maxlen (%sre-max-length (cadr sre)))
                         (pin? (not (and (number? ext) (= ext 0))))
                         (suffix (if pin?
                                     (append (cdr sre) (list '%look-behind-end))
                                     (append (cdr sre) '(eos))))
                         (check
                          (lp (sre-sequence (cons '(* any) suffix))
                              n
                              flags
                              (lambda (cnk init src str i end matches fail) i))))
                    (lambda (cnk init src str i end matches fail)
                      ;; A body of bounded width K cannot have begun more than K
                      ;; units back, so begin the rescan at (i - K).  Unknown
                      ;; width, or a scan that began in an earlier chunk: keep the
                      ;; general rescan from the chunk start.
                      (let ((i* (if (and maxlen (eq? (car init) src))
                                    (max (cdr init) (- i maxlen))
                                    (cdr init))))
                        (if (and maxlen (eq? (car init) src)
                                 (> (- i (cdr init)) maxlen))
                            (set! %look-behind-window maxlen))
                        (let* ((cnk* (cond ((eq? ext #f) cnk)
                                           ((> ext 0)
                                            (wrap-end-chunker
                                             cnk src
                                             (min (+ i ext)
                                                  ((chunker-get-end cnk) src))))
                                           (else (wrap-end-chunker cnk src i))))
                               (str* ((chunker-get-str cnk*) (car init)))
                               (end* ((chunker-get-end cnk*) (car init)))
                               (saved %look-behind-target)
                               (ok (begin
                                     (set! %look-behind-target i)
                                     ((if (eq? (car sre) 'look-behind)
                                          (lambda (x) x) not)
                                      (check cnk* init (car init) str* i* end*
                                             matches (lambda () #f))))))
                          (set! %look-behind-target saved)
                          (if ok
                              (next cnk init src str i end matches fail)
                              (fail)))))))))
            ((atomic)
             (let ((once
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (cnk init src str i end matches fail) i))))
               (lambda (cnk init src str i end matches fail)
                 (let ((j (once cnk init src str i end matches (lambda () #f))))
                   (if j
                       (next cnk init src str j end matches fail)
                       (fail))))))
            ((if)
             (let* ((test-submatches (sre-count-submatches (cadr sre)))
                    (pass (lp (caddr sre) flags (+ n test-submatches) next))
                    (fail (if (pair? (cdddr sre))
                              (lp (cadddr sre)
                                  (+ n test-submatches
                                     (sre-count-submatches (caddr sre)))
                                  flags
                                  next)
                              (lambda (cnk init src str i end matches fail)
                                (fail)))))
               (cond
                ((or (number? (cadr sre)) (symbol? (cadr sre)))
                 (let ((index
                        (if (symbol? (cadr sre))
                            (cond
                             ((assq (cadr sre) names) => cdr)
                             (else
                              (error "unknown named backref in SRE IF" sre)))
                            (cadr sre))))
                   (lambda (cnk init src str i end matches fail2)
                     (if (%irregex-match-end-chunk matches index)
                         (pass cnk init src str i end matches fail2)
                         (fail cnk init src str i end matches fail2)))))
                (else
                 (let ((test (lp (cadr sre) n flags pass)))
                   (lambda (cnk init src str i end matches fail2)
                     (test cnk init src str i end matches
                           (lambda () (fail cnk init src str i end matches fail2)))
                     ))))))
            ((backref backref-ci)
             (let ((n (cond ((number? (cadr sre)) (cadr sre))
                            ((assq (cadr sre) names) => cdr)
                            (else (error "unknown backreference" (cadr sre)))))
                   (compare (if (or (eq? (car sre) 'backref-ci)
                                    (flag-set? flags ~case-insensitive?))
                                string-ci=?
                                string=?)))
               (lambda (cnk init src str i end matches fail)
                 (let ((s (irregex-match-substring matches n)))
                   (if (not s)
                       (fail)
                       ;; XXXX create an abstract subchunk-compare
                       (let lp ((src src)
                                (str str)
                                (i i)
                                (end end)
                                (j 0)
                                (len (string-length s)))
                         (cond
                          ((<= len (- end i))
                           (cond
                            ((compare (substring s j (string-length s))
                                      (substring str i (+ i len)))
                             (next cnk init src str (+ i len) end matches fail))
                            (else
                             (fail))))
                          (else
                           (cond
                            ((compare (substring s j (+ j (- end i)))
                                      (substring str i end))
                             (let ((src2 ((chunker-get-next cnk) src)))
                               (if src2
                                   (lp src2
                                       ((chunker-get-str cnk) src2)
                                       ((chunker-get-start cnk) src2)
                                       ((chunker-get-end cnk) src2)
                                       (+ j (- end i))
                                       (- len (- end i)))
                                   (fail))))
                            (else
                             (fail)))))))))))
            ((dsm)
             (lp (sre-sequence (cdddr sre)) (+ n (cadr sre)) flags next))
            (($ submatch)
             (let ((body
                    (lp (sre-sequence (cdr sre))
                        (+ n 1)
                        flags
                        (lambda (cnk init src str i end matches fail)
                          (let ((old-source
                                 (%irregex-match-end-chunk matches n))
                                (old-index
                                 (%irregex-match-end-index matches n)))
                            (irregex-match-end-chunk-set! matches n src)
                            (irregex-match-end-index-set! matches n i)
                            (next cnk init src str i end matches
                                  (lambda ()
                                    (irregex-match-end-chunk-set!
                                     matches n old-source)
                                    (irregex-match-end-index-set!
                                     matches n old-index)
                                    (fail))))))))
               (lambda (cnk init src str i end matches fail)
                 (let ((old-source (%irregex-match-start-chunk matches n))
                       (old-index (%irregex-match-start-index matches n)))
                   (irregex-match-start-chunk-set! matches n src)
                   (irregex-match-start-index-set! matches n i)
                   (body cnk init src str i end matches
                         (lambda ()
                           (irregex-match-start-chunk-set!
                            matches n old-source)
                           (irregex-match-start-index-set!
                            matches n old-index)
                           (fail)))))))
            ((=> submatch-named)
             (rec `(submatch ,@(cddr sre))))
            (else
             (error "unknown regexp operator" sre)))))
     ((symbol? sre)
      (case sre
        ((any)
         (lambda (cnk init src str i end matches fail)
           (if (< i end)
               (next cnk init src str (+ i 1) end matches fail)
               (let ((src2 ((chunker-get-next cnk) src)))
                 (if src2
                     (let ((str2 ((chunker-get-str cnk) src2))
                           (i2 ((chunker-get-start cnk) src2))
                           (end2 ((chunker-get-end cnk) src2)))
                       (next cnk init src2 str2 (+ i2 1) end2 matches fail))
                     (fail))))))
        ((nonl)
         (lambda (cnk init src str i end matches fail)
           (if (< i end)
               (if (not (eqv? #\newline (string-ref str i)))
                   (next cnk init src str (+ i 1) end matches fail)
                   (fail))
               (let ((src2 ((chunker-get-next cnk) src)))
                 (if src2
                     (let ((str2 ((chunker-get-str cnk) src2))
                           (i2 ((chunker-get-start cnk) src2))
                           (end2 ((chunker-get-end cnk) src2)))
                       (if (not (eqv? #\newline (string-ref str2 i2)))
                           (next cnk init src2 str2 (+ i2 1) end2 matches fail)
                           (fail)))
                     (fail))))))
        ((bos)
         (lambda (cnk init src str i end matches fail)
           (if (and (eq? src (car init)) (eqv? i (cdr init)))
               (next cnk init src str i end matches fail)
               (fail))))
        ((bol)
         (lambda (cnk init src str i end matches fail)
           (if (let ((ch (if (> i ((chunker-get-start cnk) src))
                             (string-ref str (- i 1))
                             (chunker-prev-char cnk init src))))
                 (or (not ch) (eqv? #\newline ch)))
               (next cnk init src str i end matches fail)
               (fail))))
        ((bow)
         (lambda (cnk init src str i end matches fail)
           (if (and (if (> i ((chunker-get-start cnk) src))
                        (not (%word-char? (string-ref str (- i 1))))
                        (let ((ch (chunker-prev-char cnk init src)))
                          (or (not ch) (not (%word-char? ch)))))
                    (if (< i end)
                        (%word-char? (string-ref str i))
                        (let ((next ((chunker-get-next cnk) src)))
                          (and next
                               (%word-char?
                                (string-ref ((chunker-get-str cnk) next)
                                            ((chunker-get-start cnk) next)))))))
               (next cnk init src str i end matches fail)
               (fail))))
        ((eos)
         (lambda (cnk init src str i end matches fail)
           (if (and (>= i end) (not ((chunker-get-next cnk) src)))
               (next cnk init src str i end matches fail)
               (fail))))
        ((eol)
         (lambda (cnk init src str i end matches fail)
           (if (if (< i end)
                   (eqv? #\newline (string-ref str i))
                   (let ((src2 ((chunker-get-next cnk) src)))
                     (if (not src2)
                         #t
                         (eqv? #\newline
                               (string-ref ((chunker-get-str cnk) src2)
                                           ((chunker-get-start cnk) src2))))))
               (next cnk init src str i end matches fail)
               (fail))))
        ((eow)
         (lambda (cnk init src str i end matches fail)
           (if (and (if (< i end)
                        (not (%word-char? (string-ref str i)))
                        (let ((ch (chunker-next-char cnk src)))
                          (or (not ch) (not (%word-char? ch)))))
                    (if (> i ((chunker-get-start cnk) src))
                        (%word-char? (string-ref str (- i 1)))
                        ;; jolt-406: `(or (not prev) ...)` here read the ABSENCE of
                        ;; a preceding character as a word character, so every
                        ;; subject beginning with a non-word character reported a
                        ;; word ending at position 0 — (re-seq #"\b" " ab") was
                        ;; (0 1 3) against the JVM's (1 3).  There is no word
                        ;; before the start of input, so there is nothing for one
                        ;; to end.
                        (let ((prev (chunker-prev-char cnk init src)))
                          (and prev (%word-char? prev)))))
               (next cnk init src str i end matches fail)
               (fail))))
        ((nwb)  ;; non-word-boundary
         (lambda (cnk init src str i end matches fail)
           (let ((c1 (if (< i end)
                         (string-ref str i)
                         (chunker-next-char cnk src)))
                 (c2 (if (> i ((chunker-get-start cnk) src))
                         (string-ref str (- i 1))
                         (chunker-prev-char cnk init src))))
             ;; jolt-406: \B is the complement of \b, so it must answer for the
             ;; positions at the very edges too — and there it has only one
             ;; neighbour.  The old `(and c1 c2 ...)` failed outright whenever a
             ;; neighbour was missing, which made \B unmatchable at position 0 and
             ;; at the end of input: (re-seq #"\B" "ab ") was (1) against the
             ;; JVM's (1 3).  The edge of the input is not a word character, it is
             ;; the absence of one, which is exactly how a non-word character
             ;; behaves for this test — so treat a missing neighbour as non-word
             ;; and ask the real question: do both sides have the SAME wordness?
             ;; If they do there is no transition here, which is what \B means.
             (let ((w1 (and c1 (%word-char? c1)))
                   (w2 (and c2 (%word-char? c2))))
               (if (eq? w1 w2)
                   (next cnk init src str i end matches fail)
                   (fail))))))
        ((epsilon)
         next)
        ((%look-behind-end)
         ;; jolt #1062 / drg-bba2: zero-width assertion that succeeds only when the
         ;; current position equals the look-behind's target end.  The target is
         ;; set around the body's evaluation (%look-behind-target), letting a
         ;; look-behind body's end be pinned to i while its chunk is widened past i
         ;; so a nested look-around can read there.
         (lambda (cnk init src str i end matches fail)
           (if (= i %look-behind-target)
               (next cnk init src str i end matches fail)
               (fail))))
        (else
         ;; jolt #1062: java.util.regex's line anchors (host/chez/java/
         ;; regex-anchors.ss).  Each is decidable from the unit before the position
         ;; and the two after it, so it compiles to ONE zero-width test rather than
         ;; the look-around SRE it is registered as.  This arm has to precede the
         ;; sre-named-definitions lookup below, which would expand it back into
         ;; that SRE — which is exactly what every other irregex walker wants, and
         ;; what the matcher must not do.
         (cond
           ((%java-anchor-proc sre)
            => (lambda (anchor?)
                 (lambda (cnk init src str i end matches fail)
                   (if (anchor? cnk init src str i end)
                       (next cnk init src str i end matches fail)
                       (fail)))))
           ;; …and a named class (`whitespace`, `alphanumeric`, `punctuation`, …)
           ;; whose definition is an alternation of units folds to a char set for
           ;; the same reason the `or` arm above does; expanding it first would
           ;; hand the alternation chain back.
           ((%sre-cset-able? sre)
            (sre-cset->procedure
             (sre->cset sre (flag-set? flags ~case-insensitive?))
             next))
           ((assq sre sre-named-definitions) => (lambda (cell) (rec (cdr cell))))
           (else (error "unknown regexp" sre))))))
     ((char? sre)
      (if (flag-set? flags ~case-insensitive?)
          ;; case-insensitive
          (lambda (cnk init src str i end matches fail)
            (if (>= i end)
                (let lp ((src2 ((chunker-get-next cnk) src)))
                  (if src2
                      (let ((str2 ((chunker-get-str cnk) src2))
                            (i2 ((chunker-get-start cnk) src2))
                            (end2 ((chunker-get-end cnk) src2)))
                        (if (>= i2 end2)
                            (lp ((chunker-get-next cnk) src2))
                            (if (char-ci=? sre (string-ref str2 i2))
                                (next cnk init src2 str2 (+ i2 1) end2
                                      matches fail)
                                (fail))))
                      (fail)))
                (if (char-ci=? sre (string-ref str i))
                    (next cnk init src str (+ i 1) end matches fail)
                    (fail))))
          ;; case-sensitive
          (lambda (cnk init src str i end matches fail)
            (if (>= i end)
                (let lp ((src2 ((chunker-get-next cnk) src)))
                  (if src2
                      (let ((str2 ((chunker-get-str cnk) src2))
                            (i2 ((chunker-get-start cnk) src2))
                            (end2 ((chunker-get-end cnk) src2)))
                        (if (>= i2 end2)
                            (lp ((chunker-get-next cnk) src2))
                            (if (char=? sre (string-ref str2 i2))
                                (next cnk init src2 str2 (+ i2 1) end2
                                      matches fail)
                                (fail))))
                      (fail)))
                (if (char=? sre (string-ref str i))
                    (next cnk init src str (+ i 1) end matches fail)
                    (fail))))
          ))
     ((string? sre)
      (rec (sre-sequence (string->list sre)))
;; XXXX reintroduce faster string matching on chunks
;;       (if (flag-set? flags ~case-insensitive?)
;;           (rec (sre-sequence (string->list sre)))
;;           (let ((len (string-length sre)))
;;             (lambda (cnk init src str i end matches fail)
;;               (if (and (<= (+ i len) end)
;;                        (%substring=? sre str 0 i len))
;;                   (next str (+ i len) matches fail)
;;                   (fail)))))
      )
     (else
      (error "unknown regexp" sre)))))
  (if plan (%memo-root plan root) root))

;; jolt-t16: irregex's cset-union, with its merge fixed. Two char sets are sorted
;; range lists, walked in step; when the heads overlap (or touch) they are merged.
;; The vendored version always put the merged range back at the head of the FIRST
;; list, even when the second list's range reached further, so the first list's
;; later ranges were left behind INSIDE the merged one and the result came out
;; unsorted: (or (~ #\a) whitespace) built
;;   ((#\nul . #\`) (#\space . #\space) (#\b . ...) ...)
;; and the binary search over it lost characters — (re-find #"[^a]|\s" "A") was
;; nil. Every java.util.regex class with a negated or multi-range part unioned
;; with another can hit it. The merged range now goes back into whichever list
;; its high end came from, so the other list's remainder is still compared
;; against it. The `cset-union` below is otherwise the vendored procedure; `make
;; regexanchorcheck` pins the original.
(define (cset-union a b)
  (let union-range ((a (vector->list a))
                    (b (vector->list b))
                    (res '()))
    (cond
     ((null? a) (list->vector (reverse (append (reverse b) res))))
     ((null? b) (list->vector (reverse (append (reverse a) res))))
     (else
      (let ((a-range (car a))
            (b-range (car b)))
        (cond
         ;; Can't use next-char here since it will cause an error if we are
         ;; comparing a cset with the maximum character as high char.
         ((< (+ (char->integer (cdr a-range)) 1) (char->integer (car b-range)))
          (union-range (cdr a) b (cons a-range res)))
         ((> (char->integer (car a-range)) (+ (char->integer (cdr b-range)) 1))
          (union-range (cdr b) a (cons b-range res)))
         ;; jolt-t16: keep the merged range in the list it ends in.
         ((char>=? (cdr a-range) (cdr b-range))
          (union-range (cons (char-ranges-union a-range b-range) (cdr a))
                       (cdr b)
                       res))
         (else (union-range (cdr a)
                            (cons (char-ranges-union a-range b-range) (cdr b))
                            res))))))))
