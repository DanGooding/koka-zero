open Core
open Import
module EPS = Evidence_passing_syntax
module E = EPS.Expr

module State = struct
  type t = Variable.Name_source.t [@@deriving sexp_of]

  let initial ?(prefix = "mon_") () : t = Variable.Name_source.fresh ~prefix ()
end

module T = struct
  (** a state monad to keep the name source state *)
  type 'a t = State.t -> 'a Or_static_error.t

  let bind m ~f s =
    let%bind.Result x = m s in
    f x s
  ;;

  let return x _s = Result.Ok x

  let map =
    let map m ~f s =
      let%map.Result x = m s in
      f x
    in
    `Custom map
  ;;
end

module T' = struct
  include T
  include Monad.Make (T)
end

include T'
include Monad_utils.Make (T')

let run ?name_prefix (t : 'a t) : 'a Or_static_error.t =
  let state = State.initial ?prefix:name_prefix () in
  t state
;;

let unsupported_feature_error message _s =
  Static_error.unsupported_feature message |> Result.Error
;;

let fresh_name s = Variable.Name_source.next_name s |> Result.Ok

let pure_parameters_of_names xs =
  List.map xs ~f:(fun x -> Parameter.Variable x, EPS.Type.Pure)
;;

let make_lambda_1 type_ make_body =
  let open Let_syntax in
  let%bind x = fresh_name in
  let%map body = make_body (E.Variable x) in
  let ps = pure_parameters_of_names [ x ] in
  ps, type_, body
;;

(* note since we can't pattern match on the type of make_body, there isn't much
   we can do that's better than this duplication. Could use ~dependent types
   with the number of arguments passed as a natural number but probably
   overkill *)
let make_lambda_2 type_ make_body =
  let open Let_syntax in
  let%bind x1 = fresh_name in
  let%bind x2 = fresh_name in
  let%map body = make_body (E.Variable x1) (E.Variable x2) in
  let ps = pure_parameters_of_names [ x1; x2 ] in
  ps, type_, body
;;

let make_lambda_3 type_ make_body =
  let open Let_syntax in
  let%bind x1 = fresh_name in
  let%bind x2 = fresh_name in
  let%bind x3 = fresh_name in
  let%map body = make_body (E.Variable x1) (E.Variable x2) (E.Variable x3) in
  let ps = pure_parameters_of_names [ x1; x2; x3 ] in
  ps, type_, body
;;

let make_lambda_4 type_ make_body =
  let open Let_syntax in
  let%bind x1 = fresh_name in
  let%bind x2 = fresh_name in
  let%bind x3 = fresh_name in
  let%bind x4 = fresh_name in
  let%map body =
    make_body (E.Variable x1) (E.Variable x2) (E.Variable x3) (E.Variable x4)
  in
  let ps = pure_parameters_of_names [ x1; x2; x3; x4 ] in
  ps, type_, body
;;

let make_lambda_5 type_ make_body =
  let open Let_syntax in
  let%bind x1 = fresh_name in
  let%bind x2 = fresh_name in
  let%bind x3 = fresh_name in
  let%bind x4 = fresh_name in
  let%bind x5 = fresh_name in
  let%map body =
    make_body
      (E.Variable x1)
      (E.Variable x2)
      (E.Variable x3)
      (E.Variable x4)
      (E.Variable x5)
  in
  let ps = pure_parameters_of_names [ x1; x2; x3; x4; x5 ] in
  ps, type_, body
;;

let expr_of_lambda lambda = E.Lambda lambda

let make_lambda_expr_1 type_ make_body =
  make_lambda_1 type_ make_body |> map ~f:expr_of_lambda
;;

let make_lambda_expr_2 type_ make_body =
  make_lambda_2 type_ make_body |> map ~f:expr_of_lambda
;;

let make_lambda_expr_3 type_ make_body =
  make_lambda_3 type_ make_body |> map ~f:expr_of_lambda
;;

let make_lambda_expr_4 type_ make_body =
  make_lambda_4 type_ make_body |> map ~f:expr_of_lambda
;;

let make_lambda_expr_5 type_ make_body =
  make_lambda_5 type_ make_body |> map ~f:expr_of_lambda
;;

let make_match_ctl subject ~pure ~yield =
  let open Let_syntax in
  let%bind x = fresh_name in
  let%bind pure_branch_body = pure (E.Variable x) in
  let pure_branch = Parameter.Variable x, pure_branch_body in
  let%bind marker = fresh_name in
  let%bind op_clause = fresh_name in
  let%bind resumption = fresh_name in
  let%map yield_branch_body =
    yield
      ~marker:(E.Variable marker)
      ~op_clause:(E.Variable op_clause)
      ~resumption:(E.Variable resumption)
  in
  let yield_branch = marker, op_clause, resumption, yield_branch_body in
  E.Match_ctl { subject; pure_branch; yield_branch }
;;

let make_match_op subject ~normal ~tail =
  let open Let_syntax in
  let%bind f = fresh_name in
  let%bind normal_branch_body = normal (E.Variable f) in
  let normal_branch = f, normal_branch_body in
  let%bind f' = fresh_name in
  let%map tail_branch_body = tail (E.Variable f') in
  let tail_branch = f', tail_branch_body in
  E.Match_op { subject; normal_branch; tail_branch }
;;
