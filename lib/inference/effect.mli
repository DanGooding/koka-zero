open Core

(** the name of an individual effect *)
module Label : Identifiable.S

(** a variable standing for an effect, either free, or quantified in a [Type.Poly.t]*)
module Variable : sig
  type t [@@deriving sexp]

  include Identifiable.S with type t := t
  include Name_source.S with type t := t
end

(** a placeholder variable introduced during unification, an effect will be
    substituted for this *)
module Metavariable : sig
  type t [@@deriving sexp]

  include Identifiable.S with type t := t
  include Name_source.S with type t := t
end

(* are rows difference lists? *)
(* or ordinary lists?*)
module Row : sig
  (* a' = Metavariable / Variable / unit ? *)
  type 'a t =
    { labels : Label.Multiset.t
    (* TODO: really should just be a cons list (label * Effect.t option) but then
       everything gets mutauly recursive *)
    ; tail : 'a option (* TODO better representation *)
    }
    [@@deriving sexp]

    val extend : t -> Label.t -> t
    val empty : 'a t



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

(** set of operation names (used for structural matchijng to handlers) *)
module Signature : sig
  (* TODO: include number of arguments when this becomes varaiable *)
  type t = (* abstractly *) Mininmal_syntax.Variable.Set.t
  [@@deriving sexp]
end

(* forall can quantify over effect varaibles too *)
type t =
  | Metavariable of Metavariable.t
  | Variable of Variable.t
  | Row of Row.t
[@@deriving sexp]

