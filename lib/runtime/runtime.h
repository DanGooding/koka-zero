// C runtime, linked with compiled executables.
// This header provides the logical interface, even though it is
// in no way `#include`d into the generated LLVM-IR module.

#include <stdint.h>

// pointer to an arbitrary runtime value
typedef uint8_t* opaque_ptr;

typedef uint8_t bool_t;
typedef int64_t int_t;
typedef uint64_t marker_t;
typedef uint64_t label_t;

// initialises runtime garbage collection
void kkr_init(void);

// exits with failure
void kkr_exit(void);
// prints a null terminated error message, then exits with failure
void kkr_exit_with_message(const uint8_t *message);

// safe malloc wrapper: will never return NULL, instead exits on failure
opaque_ptr kkr_malloc(uint64_t size);

// generates a fresh marker
marker_t kkr_fresh_marker(void);
// compares two markers for equality
bool_t kkr_markers_equal(marker_t, marker_t);

// returns a heap allocated empty evidence vector
opaque_ptr kkr_nil_evidence_vector(void);

opaque_ptr kkr_cons_evidence_vector(
  label_t label,
  marker_t marker,
  opaque_ptr handler,
  opaque_ptr vector_tail);

// lookup the evidence entry for a given label in an evidence vector
// exits if not present
opaque_ptr kkr_evidence_vector_lookup(opaque_ptr vector, label_t label);

// access the [marker] field of an evidence entry
marker_t kkr_get_evidence_marker(opaque_ptr);
// access the [handler] field of an evidence entry
opaque_ptr kkr_get_evidence_handler(opaque_ptr);

// write the given integer (plus a newline) to stdout
void kkr_print_int(int_t);
// read and parse an integer from stdin, exits on failure
int_t kkr_read_int(void);
