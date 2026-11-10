# TARGET-CONTRACT.md — porting jolt's host layer to a new Scheme

This is the porting document. It says what a target must provide, what it may
degrade and how, which files it replaces outright, and how the gates hold the
boundary. The machine-readable inventory is CONTRACT.txt (names, shapes,
tiers) — this file is the narrative that makes it actionable. The design
history is RFC 0010 on the site.

## The shape of a port

1. **Satisfy the contract names** (CONTRACT.txt, tiers `threads` through
   `misc`). Most exist in any serious Scheme under some spelling; the work is
   matching the PINNED SEMANTICS below, not finding the functions.
2. **Implement or degrade the capability entry points** (`sa-*`,
   CONTRACT.txt `capability-*` tiers). Each has a documented degradation; an
   absent capability raises or returns empty — it never fakes.
3. **Replace the two target-owned files**: `scheme-adapter-runtime.ss` (your
   adapter IS this file for your target) and `hasheq.ss` (see below).
4. **Add a backend primitive-table entry** (jolt-core/jolt/backend_scheme.clj
   `target-prims`): your spelling for the unsafe-op prefix (empty string =
   checked ops, safe and slower) — and make the four `sa-foreign-*` syntaxes
   expand to your FFI forms.
5. **Start from `guile.ss`** — the structural stub mirrors the Chez adapter
   section by section and marks every place where "the target has this" still
   needs "and it behaves the same" verified.

## Pinned semantics (behavior a standard does not give you)

These are load-bearing. Each was audited against real call sites (the R4
threads table covered 280 of them); getting one wrong produces silent
misbehavior, not a crash.

- **Thread-parameters fork-inherit.** A child thread starts with the parent's
  current bindings for every `make-thread-parameter`. jolt's dynamic-binding
  conveyance into agent/async/future workers relies on this ALONE.
- **Mutexes are non-recursive.** jolt implements reentrancy itself (monitors
  are (mutex, owner, count) with IllegalMonitorStateException on non-owner
  exit). If your locks are recursive, that must not become load-bearing.
- **`condition-wait` may wake spuriously.** Every wait site loops on its
  predicate; your implementation may wake threads freely but must not LOSE
  wakeups.
- **An escape continuation is one-shot, and the target enforces it.**
  `sa-call-with-escape-continuation` hands `proc` a procedure valid AT MOST
  ONCE and only while that call is still on the stack. A second invocation, or
  one after `proc` returned normally, must RAISE — a target whose only
  primitive is multi-shot (`call/cc`) carries a spent flag rather than letting
  control re-enter a finished frame. The escape must unwind the dynamic-wind
  chain on its way out: jolt's `finally` runs on an escape, and a target that
  skipped the unwind would silently drop cleanup. Ownership (which thread and
  fiber may invoke a given escape) is the HOST's rule, not yours.
- **Native error capture is part of the foreign call boundary.**
  `sa-foreign-procedure-native-error` must return the C result and the calling
  thread's errno/GetLastError-equivalent atomically; a later host call that reads
  an error slot is not an equivalent implementation. A target without native
  FFI must raise its documented unsupported-capability error.
- **Blocking foreign calls must not stop other threads' GC.** Chez spells
  this `__collect_safe`; `sa-foreign-procedure-blocking` /
  `sa-foreign-callable-collect-safe` carry the requirement. A target whose
  collector never stops other threads may collapse blocking to plain.
- **The interaction environment is jolt's top level.** `def-var!` targets it
  and `(eval expr (interaction-environment))` must see it.
- **Degradation raises are message-carrying conditions** — `(error 'who
  "message")`, never a bare raise: jolt surfaces the message through
  ex-message, and a bare raise surfaces nil.

## Capability degradations, by tier

- **system**: `sa-run-process` raises without subprocess support (callers
  genuinely need the child). `sa-environment-pairs` is the whole environment
  as the R7RS `get-environment-variables` alist, read in process, never by
  running a program. `sa-os-release` may answer #f, which leaves
  `os.version` out of the system properties. GC hooks (`sa-gc-collect`,
  `sa-gc-trip-bytes!`) may no-op. Clocks (`sa-real-time-ms`) and
  `sa-file-mtime-ms` are required — do not fake them.
- **introspect**: `sa-continuation-frames` → `'()`, `sa-procedure-info` → #f,
  `sa-stats` → zero vector. The runtime keeps working: backtraces carry type
  and message without frames (the degradedbacktrace gate proves this mode),
  static frame reconstruction from callsite tables still works, and the image
  writer refuses closures instead of guessing.
- **ffi**: without dlopen/FFI, `sa-load-shared-object` raises and jolt.ffi
  surfaces it as a jolt-level error at `load-library` — the one user-facing
  choke point. `sa-lock-object`/`sa-unlock-object` may BOTH no-op on a
  non-moving collector (never just one). `sa-foreign-procedure-runtime`
  constructs an FP from a runtime signature — Chez needs eval for this; a
  target with native runtime construction (Guile `pointer->procedure`) does
  it directly.
- **native-compile**: `sa-compile-file` takes a target-neutral profile
  ((optimize . 0..3) (inspector-info . bool) (source-info . bool)
  (compressed . bool)); map what you have, ignore the rest. On raise, the AOT
  cache disables silently and everything loads from source — an
  interpretation-only target runs jolt fine, it just cannot `jolt build`.
- **image**: `sa-fasl-write`/`sa-fasl-read` raise → `jolt.image` dump/restore
  report unsupported. A target with its own serializer can implement them,
  but note the format contract: raw-traveling nongenerative record layouts
  are image-format surface.
- **profile**: `sa-profile-instrument!` answers #f for on → `jolt.profile`
  reports unsupported; the counts and releases are empty, a block-profile dump writes nothing and a unit load finds no data and `sa-profile-compile` just
  calls its thunk.
  Its `:debug` mode is refused the same way, and `jolt.debug` then finds no
  frames on a throwable and never calls a break handler.
- **sample**: `sa-stack-walk` answers #f → `jolt.profile/sample!` reports
  unsupported; frame sources and code names are #f, the thread clock 0.
- **threads**: absence is a designed mode, not a raise — THREADS.md specifies
  synchronous agents/futures per construct: what stays observably honest,
  what must raise instead of faking concurrency.

## Platform identity

Logic branches on derived properties only: `sa-os-family` ('macos | 'windows
| 'linux), `sa-arch`, `sa-endian`. The raw `sa-host-tag` string appears in
NAMES only (release dirs, image headers, telemetry) — a port chooses any
stable identifier; nothing parses it except target-owned build machinery.

The derived three answer for the process that is RUNNING, not for the build
that produced it. Chez's native machine tag happens to answer both, which is
why the adapter reads it; its portable-bytecode tags name no OS at all, and
deriving one from them called every bytecode build Linux (#796) while leaving
the architecture and byte order unknown (#798). A port whose identifier is
likewise portable must probe the host instead of parsing the identifier — the
Chez adapter probes the filesystem for the OS, `uname(2)` for the architecture,
and answers the byte order from `native-endianness` — and each answer may be
cached, since none can change while the process runs.

`sa-endian` has no degraded answer: a runtime always knows its own byte order,
so a port answers it rather than declining. `sa-arch` may still answer `'other`,
which callers read as "unverified" — but `'other` must mean the port could not
find out, never merely that its identifier did not say.

## Target-owned files

The lint's allowlist may only name these; everything else routes through the
adapter (portcheck enforces this structurally):

- `host/chez/scheme-adapter-runtime.ss` — the adapter itself; a port writes
  its own.
- `host/chez/hasheq.ss` — the Chez-tuned hash implementation (proven-sound
  `#3%` unsafe variants in the hot loops). A port reimplements its exported
  surface with safe ops first; tune later. The surface is listed in the
  file's header.

## Emitted code

The backend emits through the same boundary: unsafe primitives via the
per-target table (meaning-keyed; the chez entry reproduces today's output
byte-exactly — the minted seed prelude is the proof), FFI lowerings via the
`sa-foreign-*` syntaxes. Generated code is as portable as handwritten code;
your adapter loads first in every boot path (rt.ss owns the load), so emitted
references resolve by construction.

## The gates

- `make portcheck` — no forbidden name outside target-owned files; stale
  lines fail; allowlist lines naming other files fail structurally.
- `make adaptercheck` — every contract name and `sa-*` entry bound (syntax
  bindings included).
- `make census` — regenerates the per-tier inventory (.dirge/psl-census.md).
- `make degradedbacktrace` — the introspect-off mode actually runs.

A port is credible when: adaptercheck passes against YOUR adapter, the
corpus and unit gates pass in interpretation mode, and every degraded
capability fails the documented way rather than faking.
