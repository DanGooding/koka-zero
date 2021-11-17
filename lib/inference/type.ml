open Core

module Primitive = struct
  module T = struct
    type t =
      | Int
      | Bool
      | Unit
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

(* TODO: currently have no annotations, but will need to use a variant to
   namespace user names from generated names *)

(** a variable standing for a type, either free, or quantified in a [Poly.t]*)
module Variable = struct
  module T = String
  include T
  include Name_source.Make (T)
end

(** a placeholder variable introduced during unification, a type will be
    substituted for this *)
module Metavariable = struct
  module T = String
  include T
  include Name_source.Make (T)
end

module Mono = struct
  module T = struct
    type t =
      | Arrow of t * t
      | Variable of Variable.t
      | Metavariable of Metavariable.t
      | Primitive of Primitive.t
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

(* TODO: probably deserves its own module *)
module Poly = struct
  type t =
    { (* TODO: make these private *)
      forall_bound : Variable.Set.t
    ; monotype : Mono.t
    }
  [@@deriving sexp]
end

module T = struct
  type t =
    | Mono of Mono.t
    | Poly of Poly.t
  [@@deriving sexp]
end (* disable "fragile-match" for generated code *) [@warning "-4"]

include T
