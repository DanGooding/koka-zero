open Core
module Label : Identifiable.S

(* TODO: should these be merged with type? *)
module Metavariable : Identifiable.S
module Variable : Identifiable.S

(* are rows difference lists? *)
(* or ordinary lists?*)
module Row : sig
  (* a' = Metavariable / Variable / unit ? *)
  type 'a t =
    { labels : Label.Multiset.t
    ; tail : 'a option (* TODO better representation *)
    }
    [@@deriving sexp]

    val extend : t -> Label.t -> t
    val remove : t -> Label.t -> t option
    val contains : t -> Label.t -> bool

    val is_empty : t -> bool

    val empty : ? t



    val is_open 'a t -> bool


    (* TODO: doesn't work - recursive modules! *)
    val open : ? t -> varaible_source -> Metavaraiable.t t
    (* TODO: start without open/close*)


end


module Operation : sig
  type t =
    { argument_type : Type.Mono.t
    ; result_type : Type.Mono.t
    }
    [@@deriving sexp]
end

(* set of operation names (used for structural matchijng to handlers) *)
module Signature : sig
  type t = (* abstractly *) Operation_name_or_generic_variable.Set.t
  [@@deriving sexp]




end



(* forall can quantify over effect varaibles too *)
type t =
  | Metavariable of Metavariable.t
  | Variable of Variable.t
  | Row of Row.t
[@@deriving sexp]

