/* launcher.c — the native stub for self-contained jolt binaries (jolt-eaj).
 *
 * A toolchain-free `jolt build` (and jolt itself) produces an executable by
 * appending a Chez boot image to a copy of this prebuilt stub, framed as:
 *
 *     [stub bytes][boot bytes][boot-length : little-endian u64]["JOLTBOOT"]
 *
 * (see host/chez/java/io.ss jolt-append-payload!). At startup the stub locates
 * its own executable, reads the trailing 16-byte frame to find the boot, and
 * maps the boot read-only out of the executable itself and registers the
 * mapping, unmapped once Sbuild_heap has consumed it (where mapping is not
 * available, the Chez kernel reads the region through the fd instead). No
 * external boot file, no Chez install, and no resident copy — a malloc'd
 * payload here stayed dirty for the life of the process (7-14 MB per app). A
 * mapping's pages are clean and file-backed, and a patched kernel
 * (chez-patches/0003) uncompresses the boot straight out of them, where the fd
 * path first copies each compressed segment into the heap.
 *
 * Built once at jolt-build time against the Chez kernel (libkernel.a + scheme.h)
 * by host/chez/build-jolt.ss; the resulting binary is embedded into jolt and
 * copied per app build. Inherently per-platform (the boot targets the host
 * machine-type), like a native compiler.
 */
#include "scheme.h"
#include "jolt_zlib.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#include <fcntl.h>
#include <libproc.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
static int self_path(char *buf, uint32_t size) {
  /* _NSGetExecutablePath fills buf and reports the needed size on overflow. */
  return _NSGetExecutablePath(buf, &size);
}
static int open_self(const char *path) { return open(path, O_RDONLY); }
#elif defined(_WIN32)
#include <windows.h>
#include <io.h>
#include <fcntl.h>
static int self_path(char *buf, uint32_t size) {
  DWORD n = GetModuleFileNameA(NULL, buf, size);
  return (n == 0 || n >= size) ? -1 : 0;
}
/* A CRT fd in binary mode — the kernel reads the region with CRT reads. */
static int open_self(const char *path) { return _open(path, _O_RDONLY | _O_BINARY); }
#else
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
static int self_path(char *buf, uint32_t size) {
  ssize_t n = readlink("/proc/self/exe", buf, (size_t)size - 1);
  if (n < 0) return -1;
  buf[n] = '\0';
  return 0;
}
static int open_self(const char *path) { return open(path, O_RDONLY); }
#endif

/* Best-effort readahead of the boot region. The Chez kernel reads the boot
   through this fd during Sbuild_heap; on a cold page cache those reads block one
   after another, and nothing has told the kernel that the whole multi-MB region
   is about to be read in order. Issued before Sscheme_init so the I/O overlaps
   kernel init and the runtime image's top levels. Advisory: the result is not
   checked and a platform without an equivalent simply keeps the old timing.
   (The C-array boot sites use madvise instead — see bld-boot-prefetch-defn in
   host/chez/build.ss.) */
static void prefetch_boot_region(int fd, long off, uint64_t len) {
#if defined(__linux__)
  posix_fadvise(fd, (off_t)off, (off_t)len, POSIX_FADV_WILLNEED);
#elif defined(__APPLE__)
  /* Darwin has no posix_fadvise; F_RDADVISE is the read-ahead request, and its
     count is an int, so a boot larger than INT_MAX prefetches its first 2GB. */
  struct radvisory ra;
  ra.ra_offset = (off_t)off;
  ra.ra_count = (int)(len > (uint64_t)INT_MAX ? (uint64_t)INT_MAX : len);
  fcntl(fd, F_RDADVISE, &ra);
#else
  (void)fd;
  (void)off;
  (void)len;
#endif
}

/* Map the boot region read-only, answering the boot's first byte (and, through
   BASE/MAPLEN, what to unmap) with FD closed, or NULL where there is no mmap or
   it fails, which leaves the fd path and FD open for it. */
static void *map_boot_region(int fd, long off, uint64_t len, void **base, size_t *maplen) {
#if defined(_WIN32)
  (void)fd; (void)off; (void)len; (void)base; (void)maplen;
  return NULL;
#else
  long pagesize = sysconf(_SC_PAGESIZE);
  long start;
  void *p;
  if (pagesize <= 0 || len == 0 || len > (uint64_t)SIZE_MAX - (uint64_t)pagesize) return NULL;
  start = off - (off % pagesize);
  p = mmap(NULL, (size_t)(off - start) + (size_t)len, PROT_READ, MAP_PRIVATE, fd, (off_t)start);
  if (p == MAP_FAILED) return NULL;
  close(fd);
  *base = p;
  *maplen = (size_t)(off - start) + (size_t)len;
  return (char *)p + (off - start);
#endif
}

static void unmap_boot_region(void *base, size_t maplen) {
#if defined(_WIN32)
  (void)base; (void)maplen;
#else
  if (base != NULL) munmap(base, maplen);
#endif
}

#define JOLT_MAGIC "JOLTBOOT"
#define JOLT_MAGIC_LEN 8
#define JOLT_TRAILER_LEN 16 /* u64 length + 8-byte magic */
static double monotonic_ms(void) {
#if defined(_WIN32)
  LARGE_INTEGER frequency;
  LARGE_INTEGER counter;
  QueryPerformanceFrequency(&frequency);
  QueryPerformanceCounter(&counter);
  return (double)counter.QuadPart * 1000.0 / (double)frequency.QuadPart;
#else
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (double)now.tv_sec * 1000.0 + (double)now.tv_nsec / 1000000.0;
#endif
}

static void startup_profile_mark(int enabled, double started, double *last,
                                 const char *label) {
  if (enabled) {
    double now = monotonic_ms();
    fprintf(stderr,
            "jolt startup: [profile] native %-22s %9.3f ms"
            "   (cumulative %9.3f ms)\n",
            label, now - *last, now - started);
    *last = now;
  }
}


/* Milliseconds from process creation to now: exec, the dynamic linker binding
   the kernel and any :static natives, C constructors — everything that runs
   before main(), which no mark placed inside the program can see. This was the
   blind spot in burinc/jolt#3: a vfasl boot made every Scheme-side phase 10x
   faster while the measured cold start did not move, and the profile had
   nothing to say about where the rest went. Each platform reads its own
   process start time; -1.0 when it cannot be read, and the mark then says so
   rather than printing a made-up 0. */
static double pre_main_ms(void) {
#if defined(__linux__)
  /* /proc/self/stat field 22, starttime, in clock ticks since boot. The comm
     field (2) can hold spaces and parentheses, so parse from its closing ')'. */
  FILE *st = fopen("/proc/self/stat", "r");
  if (!st) return -1.0;
  char line[1024];
  size_t n = fread(line, 1, sizeof(line) - 1, st);
  fclose(st);
  line[n] = '\0';
  const char *p = strrchr(line, ')');
  if (!p) return -1.0;
  p++;
  unsigned long long start_ticks = 0;
  /* fields 3..21 are 19 fields after the comm; the 20th is starttime */
  for (int field = 3; field <= 22; field++) {
    while (*p == ' ') p++;
    if (field == 22) {
      if (sscanf(p, "%llu", &start_ticks) != 1) return -1.0;
      break;
    }
    while (*p && *p != ' ') p++;
  }
  long hz = sysconf(_SC_CLK_TCK);
  struct timespec now;
  if (hz <= 0 || clock_gettime(CLOCK_BOOTTIME, &now) != 0) return -1.0;
  double now_ms = (double)now.tv_sec * 1000.0 + (double)now.tv_nsec / 1000000.0;
  double start_ms = (double)start_ticks * 1000.0 / (double)hz;
  return now_ms - start_ms;
#elif defined(__APPLE__)
  struct proc_taskallinfo ti;
  if (proc_pidinfo(getpid(), PROC_PIDTASKALLINFO, 0, &ti, sizeof(ti)) != (int)sizeof(ti))
    return -1.0;
  struct timeval now;
  gettimeofday(&now, NULL);
  double now_ms = (double)now.tv_sec * 1000.0 + (double)now.tv_usec / 1000.0;
  double start_ms = (double)ti.pbsd.pbi_start_tvsec * 1000.0 +
                    (double)ti.pbsd.pbi_start_tvusec / 1000.0;
  return now_ms - start_ms;
#elif defined(_WIN32)
  FILETIME creation, exit_t, kernel_t, user_t, now;
  if (!GetProcessTimes(GetCurrentProcess(), &creation, &exit_t, &kernel_t, &user_t))
    return -1.0;
  GetSystemTimeAsFileTime(&now);
  ULARGE_INTEGER c, n;
  c.LowPart = creation.dwLowDateTime; c.HighPart = creation.dwHighDateTime;
  n.LowPart = now.dwLowDateTime;      n.HighPart = now.dwHighDateTime;
  return (double)(n.QuadPart - c.QuadPart) / 10000.0; /* 100ns units */
#else
  return -1.0;
#endif
}

#if defined(__APPLE__) && defined(__aarch64__)
#include <pthread.h>
/* A fork()ed child runs on a copy of the forking thread, and XNU gives that
   copy the thread's MAP_JIT view (pthread_jit_write_protect_np) as the kernel
   last saved it at a context switch, not its live value. Chez enables code
   writes for the length of a collection; a thread descheduled inside one that
   re-protected afterwards without another switch forks a child whose code
   pages are writable and not executable, and the child's first instruction
   back in Scheme faults (SIGBUS, "invalid memory reference"). The child
   re-protects before it returns to Scheme. chez-patches/0006 does the same in
   the kernel; this covers a stock Chez. */
static void jolt_fork_child_reprotect_code(void) { pthread_jit_write_protect_np(1); }
#define JOLT_FORK_REPROTECT() pthread_atfork(NULL, NULL, jolt_fork_child_reprotect_code)
#else
#define JOLT_FORK_REPROTECT() ((void)0)
#endif

int main(int argc, char *argv[]) {
  JOLT_FORK_REPROTECT();
  int startup_profile = getenv("JOLT_STARTUP_PROFILE") != NULL;
  double startup_started = startup_profile ? monotonic_ms() : 0.0;
  double startup_last = startup_started;
  if (startup_profile) {
    /* Fold the pre-main phase into the clock, so every cumulative figure below
       is time since the PROCESS started and the last one is the whole run. The
       clock is moved back BEFORE the first mark, so that mark's own column
       reads the phase (now - last) rather than the 0 of a clock read twice. */
    double pre = pre_main_ms();
    if (pre >= 0.0) {
      startup_started -= pre;
      startup_last = startup_started;
      startup_profile_mark(startup_profile, startup_started, &startup_last,
                           "pre-main (exec+ld)");
    } else {
      fprintf(stderr, "jolt startup: [profile] native %-22s %9s"
                      "   (not readable on this platform)\n",
              "pre-main (exec+ld)", "n/a");
    }
  }
  char path[4096];
  if (self_path(path, (uint32_t)sizeof(path)) != 0) {
    fprintf(stderr, "jolt: cannot resolve own executable path\n");
    return 1;
  }

  FILE *f = fopen(path, "rb");
  if (!f) { fprintf(stderr, "jolt: cannot open self for reading\n"); return 1; }

  if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return 1; }
  long fsize = ftell(f);
  if (fsize < JOLT_TRAILER_LEN) {
    fprintf(stderr, "jolt: no boot payload (run was not produced by jolt build)\n");
    fclose(f);
    return 1;
  }

  unsigned char trailer[JOLT_TRAILER_LEN];
  if (fseek(f, fsize - JOLT_TRAILER_LEN, SEEK_SET) != 0 ||
      fread(trailer, 1, JOLT_TRAILER_LEN, f) != JOLT_TRAILER_LEN) {
    fclose(f);
    return 1;
  }
  if (memcmp(trailer + 8, JOLT_MAGIC, JOLT_MAGIC_LEN) != 0) {
    fprintf(stderr, "jolt: boot payload not found\n");
    fclose(f);
    return 1;
  }

  uint64_t boot_len = 0;
  for (int i = 0; i < 8; i++)
    boot_len |= ((uint64_t)trailer[i]) << (8 * i);

  long boot_off = fsize - JOLT_TRAILER_LEN - (long)boot_len;
  if (boot_off < 0) {
    fprintf(stderr, "jolt: corrupt boot payload\n");
    fclose(f);
    return 1;
  }
  fclose(f);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "locate boot payload");

  int fd = open_self(path);
  if (fd < 0) {
    fprintf(stderr, "jolt: cannot reopen self for boot\n");
    return 1;
  }

  prefetch_boot_region(fd, boot_off, boot_len);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "prefetch boot payload");

  void *map_base = NULL;
  size_t map_len = 0;
  void *boot = map_boot_region(fd, boot_off, boot_len, &map_base, &map_len);

  Sscheme_init(0);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "Sscheme_init");
  if (boot != NULL) {
    Sregister_boot_file_bytes("jolt", boot, (iptr)boot_len);
  } else {
    /* final arg: close the fd when the boot is consumed */
    Sregister_boot_file_fd_region("jolt", fd, (iptr)boot_off, (iptr)boot_len, 1);
  }
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "register boot payload");
  Sbuild_heap(0, jolt_register_zlib);
  /* the heap holds copies of everything; nothing points into the boot now */
  unmap_boot_region(map_base, map_len);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "Sbuild_heap");
  int status = Sscheme_start(argc, (const char **)argv);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "Sscheme_start");
  Sscheme_deinit();
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "Sscheme_deinit");
  return status;
}
