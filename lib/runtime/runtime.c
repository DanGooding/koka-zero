#include <stdlib.h>
#include <stdio.h>
#include "gc.h"
#include "runtime.h"

void kkr_init(void) {
  GC_INIT();
}

void kkr_exit(void) {
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
  opaque_ptr p = GC_MALLOC(size);
  if (p == NULL) {
    snprintf(malloc_error_message, MALLOC_ERROR_MESSAGE_CAPACITY,
             "failed to malloc %ld bytes", size);
    kkr_exit_with_message((uint8_t *)malloc_error_message);
  }
  return p;
}

marker_t next_marker = 0;
marker_t kkr_fresh_marker(void) {
  return next_marker++;
}

// C operators can't be trusted to produce exactly these values
const bool_t const_false = 0;
const bool_t const_true = 1;

bool_t kkr_markers_equal(marker_t m1, marker_t m2) {
  return m1 == m2 ? const_true : const_false;
}

// evidence vectors and entries - these are managed entirely in the C runtime
// and are essentially abstract outside of it
typedef struct {
  opaque_ptr handler;
  marker_t marker;
  struct vector_t handler_site_vector;

} evidence_t;

typedef struct vector_t {
  uint8_t is_nil;
  label_t label;
  evidence_t *evidence;
  struct vector_t *tail;
} vector_t;


opaque_ptr kkr_nil_evidence_vector(void) {
  vector_t *vector = (vector_t *)kkr_malloc(sizeof(vector_t));
  // zero other fields for safety (fail fast on bugs)
  *vector = (vector_t) { .is_nil = 1, .label = -1, .evidence = NULL, .tail = NULL };
  return (opaque_ptr)vector;
}

opaque_ptr kkr_cons_evidence_vector(
  label_t label,
  marker_t marker,
  opaque_ptr handler,
  opaque_ptr handler_site_vector,
  opaque_ptr vector_tail
) {
  evidence_t *evidence = (evidence_t *)kkr_malloc(sizeof(evidence_t));
  *evidence = (evidence_t) {
    .handler = handler,
    .marker = marker,
    .handler_site_vector = (vector_t *)handler_site_vector
  };

  vector_t *vector = (vector_t *)kkr_malloc(sizeof(vector_t));
  *vector = (vector_t) {
    .is_nil = 0,
    .label = label,
    .evidence = evidence,
    .tail = (vector_t *)vector_tail
  };

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
  return NULL; // unreachable
}


marker_t kkr_get_evidence_marker(opaque_ptr e) {
  evidence_t *evidence = (evidence_t *)e;
  return evidence->marker;
}

opaque_ptr kkr_get_evidence_handler(opaque_ptr e) {
  evidence_t *evidence = (evidence_t *)e;
  return evidence->handler;
}

opaque_ptr kkr_get_evidence_handler_site_vector(opaque_ptr e) {
  evidence_t *evidence = (evidence_t *)e;
  return (opaque_ptr *)(evidence->handler_site_vector);
}

void kkr_print_int(int_t i) {
  printf("%ld\n", i);
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
  if (sscanf(line, "%ld\n", &result) != 1) {
    free(line);
    kkr_exit_with_message((uint8_t *)"failed to parse int");
  }
  free(line);
  return result;
}

