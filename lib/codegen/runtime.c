
// is everything just a void pointer?
typedef void* object_p;

typedef char * effect_label_p;

typedef struct {
  object_p marker;
  object_p handler;

} evidence_t; // TODO: want pointer, not value itself!

typedef struct {
  bool is_nil;
  union {
    // nil
    struct {};  // TODO: can be omitted - don't need the union
    // cons
    struct {
      // TODO: the C part at least can be well typed - then expose untyped wrappers
      object_p label;
      object_p evidence;
      object_p tail;
      // :(
    };
  };
} evidence_vector_t;


const evidence_vector_t evv_nil = { .is_nil = true; };
const object_p evv_nil = &evv_nil;

evidence_vector_t *evv_cons(const evidence_vector_t *vector, const effect_label_p label, const int * /* ?? */ marker, const object_p handler) {

};

evidence_t *evv_lookup(evidence_vector_t *vector, effect_label_p label) {

};

object_p evidence_get_marker(evidence_t *evidence) {
  return evidence->marker;
};

object_p evidence_get_handler(evidence_t *evidence) {
  return evidence->handler;
};
