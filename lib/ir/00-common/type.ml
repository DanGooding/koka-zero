open Core

module Variable = struct
  module T = struct
    include String

    let of_generated_name s = s
  end

  include T
  module Name_source = Name_source.Make (T)
end

module Metavariable = struct
  module T = struct
    include String

    let of_generated_name s = s
  end

  include T
  module Name_source = Name_source.Make (T)
end

module Primitive = struct
  module T = struct
    type t =
      | Int
      | Bool
      | Unit
    [@@deriving equal, compare, sexp_of, hash]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

(* TODO: currently have no annotations, but will need to use a variant to
   namespace user names from generated names *)

module Mono = struct
  module T = struct
    type t =
      | Arrow of t list * Effect.t * t
      | Metavariable of Metavariable.t
      | Primitive of Primitive.t
    [@@deriving sexp_of, compare, hash]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let rec max_level t ~type_metavariable_level ~effect_metavariable_level =
    match t with
    | Primitive _ -> 0
    | Metavariable meta -> type_metavariable_level meta
    | Arrow (args, effect_, result) ->
      let type_levels =
        List.map
          (result :: args)
          ~f:(max_level ~type_metavariable_level ~effect_metavariable_level)
      in
      let effect_level =
        Effect.max_level effect_ ~metavariable_level:effect_metavariable_level
      in
      List.max_elt (effect_level :: type_levels) ~compare:[%compare: int]
      |> Option.value ~default:0
  ;;
end

module Poly = struct
  type t =
    { forall_bound : Metavariable.t -> bool
    ; forall_bound_effects : Effect.Metavariable.t -> bool
    ; monotype : Mono.t
    }
  [@@deriving sexp_of]
end

type t =
  | Mono of Mono.t
  | Poly of Poly.t
[@@deriving sexp_of]

let generalise
      (monotype : Mono.t)
      ~should_generalise_type_metavariable
      ~should_generalise_effect_metavariable
  : Poly.t
  =
  { monotype
  ; forall_bound = should_generalise_type_metavariable
  ; forall_bound_effects = should_generalise_effect_metavariable
  }
;;
