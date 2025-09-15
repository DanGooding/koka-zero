#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef ENABLE_GC
#include "gc.h"
#endif

#ifdef ENABLE_RUN_STATS
#include <sys/resource.h>
#include <time.h>
#include <unistd.h>
#endif

#include "runtime.h"

#ifdef ENABLE_RUN_STATS
static const char *const kkr_run_stats_env_var = "KOKA_WRITE_RUN_STATS";
static struct timespec start_monotonic_time;
static long total_stdin_wait_time_ns = 0;
#endif

void kkr_init(void) {
#ifdef ENABLE_RUN_STATS
  if (getenv(kkr_run_stats_env_var) != NULL) {
    int err = clock_gettime(CLOCK_MONOTONIC, &start_monotonic_time);
    if (err != 0) {
      fprintf(stderr, "kkr_init: clock_gettime failed\n");
      exit(1);
    }
  }
#endif
#ifdef ENABLE_GC
  GC_INIT();
#endif
}

#ifdef ENABLE_RUN_STATS
void kkr_report_run_stats(void) {
  char *output_filename = getenv(kkr_run_stats_env_var);
  if (output_filename == NULL) return;

  // user & sytem cpu time
  struct rusage usage_stats;
  if (getrusage(RUSAGE_SELF, &usage_stats) != 0) {
    fprintf(stderr, "kkr_report_run_stats: getrusage failed\n");
    return;
  }

  long user_time_ns = usage_stats.ru_utime.tv_sec * 1000000000L +
                      usage_stats.ru_utime.tv_usec * 1000L;
  long sys_time_ns = usage_stats.ru_stime.tv_sec * 1000000000L +
                     usage_stats.ru_stime.tv_usec * 1000L;

  // real elapsed time
  struct timespec finish_monotonic_time;
  int err = clock_gettime(CLOCK_MONOTONIC, &finish_monotonic_time);
  if (err != 0) {
    fprintf(stderr, "kkr_report_run_stats: clock_gettime failed\n");
    return;
  }

  long elapsed_time_ns =
      (finish_monotonic_time.tv_sec - start_monotonic_time.tv_sec) *
          1000000000L +
      (finish_monotonic_time.tv_nsec - start_monotonic_time.tv_nsec);

  FILE *output_file = fopen(output_filename, "w");
  if (output_file == NULL) {
    fprintf(stderr, "kkr_report_run_stats: failed to open %s\n",
            output_filename);
    return;
  }

  int written = fprintf(output_file,
                        "{ \"user_time_ns\": %ld, \"sys_time_ns\": %ld, "
                        "\"elapsed_time_ns\": %ld, "
                        "\"stdin_wait_time_ns\": %ld }\n",
                        user_time_ns, sys_time_ns, elapsed_time_ns,
                        total_stdin_wait_time_ns);
  if (written < 0) {
    fprintf(stderr, "kkr_report_run_stats: failed to write to %s\n",
            output_filename);
  }
  fclose(output_file);
}
#endif

void kkr_on_finish(void) {
#ifdef ENABLE_RUN_STATS
  kkr_report_run_stats();
#endif
}

void kkr_exit(void) {
  kkr_on_finish();
  exit(1);
}

void kkr_exit_with_message(const uint8_t *message) {
  fprintf(stderr, "runtime error: %s\n", message);
  kkr_exit();
}

// statically allocated, since if malloc fails we will probably be
// unable to allocate this
#define MALLOC_ERROR_MESSAGE_CAPACITY 100
char malloc_error_message[MALLOC_ERROR_MESSAGE_CAPACITY];

opaque_ptr kkr_malloc(uint64_t size) {
#ifdef ENABLE_GC
  opaque_ptr p = GC_MALLOC(size);
#else
  opaque_ptr p = malloc(size);
#endif
  if (p == NULL) {
    snprintf(malloc_error_message, MALLOC_ERROR_MESSAGE_CAPACITY,
             "failed to malloc %" PRIu64 " bytes", size);
    kkr_exit_with_message((uint8_t *)malloc_error_message);
  }
  return p;
}

marker_t next_marker = 0;
marker_t kkr_fresh_marker(void) { return next_marker++; }

// C operators can't be trusted to produce exactly these values
const bool_t const_false = 0;
const bool_t const_true = 1;

// evidence vectors - these are managed entirely in the C runtime
// and are essentially abstract outside of it
typedef struct vector_t {
  uint8_t is_nil;  // not bool_t since this isn't a Koka value
  label_t label;
  opaque_ptr evidence;
  struct vector_t *tail;
} vector_t;

opaque_ptr kkr_nil_evidence_vector(void) {
  vector_t *vector = (vector_t *)kkr_malloc(sizeof(vector_t));
  // zero other fields for safety (fail fast on bugs)
  *vector =
      (vector_t){.is_nil = 1, .label = -1, .evidence = NULL, .tail = NULL};
  return (opaque_ptr)vector;
}

opaque_ptr kkr_cons_evidence_vector(label_t label, opaque_ptr evidence,
                                    opaque_ptr vector_tail) {
  vector_t *vector = (vector_t *)kkr_malloc(sizeof(vector_t));
  *vector = (vector_t){.is_nil = 0,
                       .label = label,
                       .evidence = evidence,
                       .tail = (vector_t *)vector_tail};

  return (opaque_ptr)vector;
}

opaque_ptr kkr_evidence_vector_lookup(opaque_ptr v, label_t label) {
  vector_t *current = (vector_t *)v;
  while (!current->is_nil) {
    if (current->label == label) {
      return (opaque_ptr)(current->evidence);
    }
    current = current->tail;
  }
  kkr_exit_with_message((uint8_t *)"effect label not found in evidence vector");
  return NULL;  // unreachable
}

void kkr_println(void) { printf("\n"); }

void kkr_print_int(int_t i, uint8_t newline) {
  if (newline)
    printf("%" PRId64 "\n", i);
  else
    printf("%" PRId64 " ", i);
}

int_t kkr_read_int(void) {
#ifdef ENABLE_RUN_STATS
  struct timespec read_stdin_start_time;
  int clock_err = clock_gettime(CLOCK_MONOTONIC, &read_stdin_start_time);
#endif

  int_t result;
  printf("input> ");
  fflush(stdout);
  char *line = NULL;
  size_t size = 0;
  if (getline(&line, &size, stdin) < 0) {
    free(line);
    kkr_exit_with_message((uint8_t *)"failed to read input line");
  }
  if (sscanf(line, "%" SCNd64 "\n", &result) != 1) {
    free(line);
    kkr_exit_with_message((uint8_t *)"failed to parse int");
  }
  free(line);

#ifdef ENABLE_RUN_STATS
  if (!clock_err) {
    struct timespec read_stdin_end_time;
    clock_err = clock_gettime(CLOCK_MONOTONIC, &read_stdin_end_time);
    if (!clock_err) {
      long wait_time_ns =
          (read_stdin_end_time.tv_sec - read_stdin_start_time.tv_sec) *
              1000000000L +
          (read_stdin_end_time.tv_nsec - read_stdin_start_time.tv_nsec);
      total_stdin_wait_time_ns += wait_time_ns;
    }
  }
#endif
  return result;
}
