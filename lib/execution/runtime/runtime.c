#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef ENABLE_GC
#include "gc.h"
#endif
#include "runtime.h"

void kkr_init(void) {
#ifdef ENABLE_GC
  GC_INIT();
#endif
}

void kkr_exit(void) { exit(1); }

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
  return result;
}
