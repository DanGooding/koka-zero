open! Core
open! Import

type t =
  { (* TODO: name_source should mutate internally *)
    mutable type_metavariable_source : Type.Metavariable.Name_source.t
  ; mutable effect_metavariable_source : Effect.Metavariable.Name_source.t
  ; constraints : Constraints.t
  ; type_metavariable_levels : int Type.Metavariable.Table.t
  ; effect_metavariable_levels : int Effect.Metavariable.Table.t
  }

let create () =
  let type_metavariable_source =
    Type.Metavariable.Name_source.fresh () ~prefix:"tm"
  in
  let effect_metavariable_source =
    Effect.Metavariable.Name_source.fresh () ~prefix:"em"
  in
  let constraints = Constraints.create () in
  let type_metavariable_levels = Type.Metavariable.Table.create () in
  let effect_metavariable_levels = Effect.Metavariable.Table.create () in
  { type_metavariable_source
  ; effect_metavariable_source
  ; constraints
  ; type_metavariable_levels
  ; effect_metavariable_levels
  }
;;

let fresh_type_metavariable t ~level : Type.Metavariable.t =
  let meta, name_source =
    Type.Metavariable.Name_source.next_name t.type_metavariable_source
  in
  t.type_metavariable_source <- name_source;
  Hashtbl.add_exn t.type_metavariable_levels ~key:meta ~data:level;
  meta
;;

let fresh_type t ~level : Type.Mono.t =
  Metavariable (fresh_type_metavariable t ~level)
;;

let fresh_effect_metavariable t ~level : Effect.Metavariable.t =
  let meta, name_source =
    Effect.Metavariable.Name_source.next_name t.effect_metavariable_source
  in
  t.effect_metavariable_source <- name_source;
  Hashtbl.add_exn t.effect_metavariable_levels ~key:meta ~data:level;
  meta
;;

let fresh_effect t ~level : Effect.t =
  Unknown (Metavariable (fresh_effect_metavariable t ~level))
;;

let union_effects t effects_ ~level =
  let overall_effect = fresh_effect t ~level in
  List.iter effects_ ~f:(fun effect_ ->
    Constraints.constrain_effect_at_most_exn
      t.constraints
      effect_
      overall_effect);
  overall_effect
;;

let operand_type : Operator.t -> Type.Primitive.t = function
  | Operator.Int _ -> Type.Primitive.Int
  | Operator.Bool _ -> Type.Primitive.Bool
;;

let operator_result_type : Operator.t -> Type.Primitive.t = function
  | Operator.Bool Operator.Bool.(And | Or) -> Type.Primitive.Bool
  | Operator.Int Operator.Int.(Plus | Minus | Times | Divide | Modulo) ->
    Type.Primitive.Int
  | Operator.Int
      Operator.Int.(
        ( Equals
        | Not_equal
        | Less_than
        | Less_equal
        | Greater_than
        | Greater_equal )) -> Type.Primitive.Bool
;;

let unary_operand_type : Operator.Unary.t -> Type.Primitive.t = function
  | Operator.Unary.Bool Operator.Bool.Unary.Not -> Type.Primitive.Bool
;;

let unary_operator_result_type : Operator.Unary.t -> Type.Primitive.t = function
  | Operator.Unary.Bool Operator.Bool.Unary.Not -> Type.Primitive.Bool
;;

(** determine the type and effect of an expression, raising if we encounter a type-error.
    This will update [constraints], and won't fully solve them. *)
let rec infer_expr_exn
          (t : t)
          (expr : Explicit_syntax.Expr.t)
          ~(env : Context.t)
          ~(level : int)
  : Type.Mono.t * Effect.t
  =
  match expr with
  | Value value ->
    let type_ = infer_value_exn t value ~env ~level in
    let effect_ = Effect.Labels Effect.Label.Set.empty in
    type_, effect_
  | Let (name, subject, body) ->
    let local_level = level + 1 in
    let type_subject = infer_value_exn t subject ~env ~level:local_level in
    let poly_subject =
      (* generalise only metavariables introduced within [subject] *)
      Type.generalise
        type_subject
        ~should_generalise_type_metavariable:(fun meta ->
          Hashtbl.find_exn t.type_metavariable_levels meta >= local_level)
        ~should_generalise_effect_metavariable:(fun meta ->
          Hashtbl.find_exn t.effect_metavariable_levels meta >= local_level)
        ~fresh_type_variable
        ~fresh_effect_variable
    in
    let env = Context.extend env ~name ~type_:(Poly poly_subject) in
    infer_expr_exn t body ~env ~level
  | Let_mono (name, subject, body) ->
    let type_subject, effect_subject = infer_expr_exn t subject ~env ~level in
    let env = Context.extend env ~name ~type_:(Mono type_subject) in
    let type_body, effect_body = infer_expr_exn t body ~env ~level in
    let overall_effect =
      union_effects t [ effect_subject; effect_body ] ~level
    in
    type_body, overall_effect
  | Application (f, args) ->
    let arg_types_and_effects =
      List.map args ~f:(fun arg -> infer_expr_exn t arg ~env ~level)
    in
    let arg_types, arg_effects = List.unzip arg_types_and_effects in
    let function_type, function_expr_effect = infer_expr_exn t f ~env ~level in
    let result : Type.Mono.t = fresh_type t ~level in
    let overall_effect =
      union_effects t (function_expr_effect :: arg_effects) ~level
    in
    Constraints.constrain_type_at_most_exn
      t.constraints
      function_type
      (Arrow (arg_types, overall_effect, result));
    result, overall_effect
  | Seq (first, second) ->
    let _type, first_effect = infer_expr_exn t first ~env ~level in
    let second_type, second_effect = infer_expr_exn t second ~env ~level in
    let overall_effect =
      union_effects t [ first_effect; second_effect ] ~level
    in
    second_type, overall_effect
  | If_then_else (e_cond, e_true, e_false) ->
    let type_cond, effect_cond = infer_expr_exn t e_cond ~env ~level in
    let type_true, effect_true = infer_expr_exn t e_true ~env ~level in
    let type_false, effect_false = infer_expr_exn t e_false ~env ~level in
    Constraints.constrain_type_at_most_exn
      t.constraints
      type_cond
      (Primitive Bool);
    let result_type = fresh_type t ~level in
    Constraints.constrain_type_at_most_exn t.constraints type_true result_type;
    Constraints.constrain_type_at_most_exn t.constraints type_false result_type;
    let overall_effect =
      union_effects t [ effect_cond; effect_true; effect_false ] ~level
    in
    result_type, overall_effect
  | Operator (left, op, right) ->
    let arg_type = operand_type op |> Type.Mono.Primitive in
    let result_type = operator_result_type op |> Type.Mono.Primitive in
    let left_type, left_effect = infer_expr_exn t left ~env ~level in
    let right_type, right_effect = infer_expr_exn t right ~env ~level in
    Constraints.constrain_type_at_most_exn t.constraints left_type arg_type;
    Constraints.constrain_type_at_most_exn t.constraints right_type arg_type;
    let overall_effect = union_effects t [ left_effect; right_effect ] ~level in
    result_type, overall_effect
  | Unary_operator (uop, arg) ->
    let op_arg_type = unary_operand_type uop |> Type.Mono.Primitive in
    let result_type = unary_operator_result_type uop |> Type.Mono.Primitive in
    let arg_type, effect_ = infer_expr_exn t arg ~env ~level in
    Constraints.constrain_type_at_most_exn t.constraints arg_type op_arg_type;
    result_type, effect_
  | Impure_built_in _ -> failwith "todo: impure builtin"

and infer_value_exn (t : t) (value : Explicit_syntax.Expr.value) ~env ~level
  : Type.Mono.t
  =
  match value with
  | Variable name ->
    let type_ = Context.get_exn env name in
    (match (type_ : Type.t) with
     | Mono mono -> mono
     | Poly poly ->
       Type.instantiate
         poly
         ~fresh_type_metavariable:(fun () -> fresh_type_metavariable t ~level)
         ~fresh_effect_metavariable:(fun () ->
           fresh_effect_metavariable t ~level))
  | Lambda (params, body) ->
    let param_metas =
      List.map params ~f:(fun param -> param, fresh_type t ~level)
    in
    let env =
      List.fold param_metas ~init:env ~f:(fun env (param, meta) ->
        match (param : Parameter.t) with
        | Wildcard -> env
        | Variable name -> Context.extend env ~name ~type_:(Mono meta))
    in
    let result, effect_ = infer_expr_exn t body ~env ~level in
    let param_types = List.map param_metas ~f:(fun (_, meta) -> meta) in
    Type.Mono.Arrow (param_types, effect_, result)
  | Fix_lambda (name, lambda) ->
    let meta_self = fresh_type t ~level in
    let env = Context.extend env ~name ~type_:(Mono meta_self) in
    let lambda_type = infer_value_exn t (Lambda lambda) ~env ~level in
    (* [lambda] should be usable as [f_self] *)
    Constraints.constrain_type_at_most_exn t.constraints lambda_type meta_self;
    lambda_type
  | Literal lit -> type_literal lit
  | Perform _ -> failwith "todo: perform"
  | Handler _ -> failwith "todo: handler"

and type_literal (lit : Literal.t) : Type.Mono.t =
  match lit with
  | Int _ -> Primitive Int
  | Bool _ -> Primitive Bool
  | Unit -> Primitive Unit
;;

let%expect_test "inference for a simple function" =
  let expr : Explicit_syntax.Expr.t =
    Value
      (Lambda
         ( [ Variable (Variable.of_user "x")
           ; Variable (Variable.of_user "f")
           ; Variable (Variable.of_user "y")
           ]
         , Operator
             ( Value (Variable (Variable.of_user "x"))
             , Int Plus
             , Application
                 ( Value (Variable (Variable.of_user "f"))
                 , [ Value (Variable (Variable.of_user "y")) ] ) ) ))
  in
  let inference = create () in
  let type_, effect_ =
    infer_expr_exn inference expr ~env:Context.empty ~level:0
  in
  print_s [%message (type_ : Type.Mono.t) (effect_ : Effect.t)];
  [%expect
    {|
    ((type_
      (Arrow ((Metavariable tm0) (Metavariable tm1) (Metavariable tm2))
       (Unknown (Metavariable em1)) (Primitive Int)))
     (effect_ (Labels ())))
    |}];
  print_s [%sexp (inference.constraints : Constraints.t)];
  [%expect
    {|
    ((type_constraints
      ((tm0 ((lowerBounds ()) (upperBounds ((Primitive Int)))))
       (tm1
        ((lowerBounds ())
         (upperBounds
          ((Arrow ((Metavariable tm2)) (Unknown (Metavariable em0))
            (Metavariable tm3))))))
       (tm3 ((lowerBounds ()) (upperBounds ((Primitive Int)))))))
     (effect_constraints
      ((em0
        ((lowerBounds ((Labels ())))
         (upperBounds ((Unknown (Metavariable em1))))))
       (em1 ((lowerBounds ((Labels ()))) (upperBounds ())))))
     (already_seen_constraints
      ((Type_at_most (type_lo (Metavariable tm0)) (type_hi (Primitive Int)))
       (Type_at_most (type_lo (Metavariable tm1))
        (type_hi
         (Arrow ((Metavariable tm2)) (Unknown (Metavariable em0))
          (Metavariable tm3))))
       (Type_at_most (type_lo (Metavariable tm3)) (type_hi (Primitive Int)))
       (Effect_at_most (effect_lo (Unknown (Metavariable em0)))
        (effect_hi (Unknown (Metavariable em1))))
       (Effect_at_most (effect_lo (Labels ()))
        (effect_hi (Unknown (Metavariable em0))))
       (Effect_at_most (effect_lo (Labels ()))
        (effect_hi (Unknown (Metavariable em1)))))))
    |}];
  let expansion = Expansion.create ~constraints:inference.constraints in
  let polar_type = Expansion.expand_type expansion type_ in
  let polar_effect = Expansion.expand_effect expansion effect_ in
  print_s
    [%message (polar_type : Polar_type.t) (polar_effect : Polar_type.Effect.t)];
  [%expect
    {|
    ((polar_type
      (Arrow
       ((Intersection ((Primitive Int)))
        (Intersection
         ((Arrow ((Variable t2)) (Intersection ((Variable e1)))
           (Intersection ((Primitive Int))))))
        (Variable t2))
       (Variable e1) (Primitive Int)))
     (polar_effect (Labels ())))
    |}]
;;
