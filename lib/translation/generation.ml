open Core
module M = Monadic_syntax
module E = M.Expr

module State = struct
  type t = M.Variable.Name_source.t [@@deriving sexp]

  let initial : t =
    (* TODO: make sure [Variable.of_generated] is used *)
    M.Variable.Name_source.fresh ~prefix:"mon_"
  ;;
end

module T = struct
  (** a state monad to keep the name source state *)
  type 'a t = State.t -> 'a * State.t

  let bind m ~f s =
    let x, s' = m s in
    f x s'
  ;;

  let return x s = x, s

  let map =
    let map m ~f s =
      let x, s' = m s in
      f x, s'
    in
    `Custom map
  ;;
end

include T
include Monad.Make (T)

let fresh_name = M.Variable.Name_source.next_name
let make_lambda_0 body = return (E.Lambda ([], body))

let make_lambda_1 make_body =
  let open Let_syntax in
  let%bind x = fresh_name in
  let%map body = make_body (E.Variable x) in
  E.Lambda ([ x ], body)
;;

(* note since we can't pattern match on the type of make_body, there isn't much
   we can do that's better than this duplication. Could use ~dependent types
   with the number of arguments passed as a natural number but probably
   overkill *)
let make_lambda_2 make_body =
  let open Let_syntax in
  let%bind x1 = fresh_name in
  let%bind x2 = fresh_name in
  let%map body = make_body (E.Variable x1) (E.Variable x2) in
  E.Lambda ([ x1; x2 ], body)
;;

let make_lambda_3 make_body =
  let open Let_syntax in
  let%bind x1 = fresh_name in
  let%bind x2 = fresh_name in
  let%bind x3 = fresh_name in
  E.Lambda
    ([ x1; x2; x3 ] make_body (E.Variable x1) (E.Variable x2) (E.Variable x3))
;;

let make_lambda_4 make_body =
  let open Let_syntax in
  let%bind x1 = fresh_name in
  let%bind x2 = fresh_name in
  let%bind x3 = fresh_name in
  let%bind x4 = fresh_name in
  let%map body =
    make_body (E.Variable x1) (E.Variable x2) (E.Variable x3) (E.Variable x4)
  in
  E.Lambda ([ x1; x2; x3; x4 ], body)
;;

let expr_of_lambda lambda = E.Lambda lambda

let make_lambda_expr_0 make_body =
  make_lambda_0 make_body |> map ~f:expr_of_lambda
;;

let make_lambda_expr_1 make_body =
  make_lambda_1 make_body |> map ~f:expr_of_lambda
;;

let make_lambda_expr_2 make_body =
  make_lambda_2 make_body |> map ~f:expr_of_lambda
;;

let make_lambda_expr_3 make_body =
  make_lambda_3 make_body |> map ~f:expr_of_lambda
;;

let make_lambda_expr_4 make_body =
  make_lambda_4 make_body |> map ~f:expr_of_lambda
;;

let make_match_ctl ~subject ~pure ~yield =
  let open Let_syntax in
  let%bind x = fresh_name in
  let%bind pure_branch_body = pure (M.Variable x) in
  let pure_branch = [ x ], pure_branch_body in
  let%bind marker = fresh_name in
  let%bind op_clause = fresh_name in
  let%bind resumption = fresh_name in
  let%map yield_branch_body =
    yield
      ~marker:(M.Variable marker)
      ~op_clause:(M.Variable op_clause)
      ~resumption:(M.Variable resumption)
  in
  let yield_branch = [ marker; op_clause; resumption ], yield_branch_body in
  E.Match_ctl { subject; pure_branch; yield_branch }
;;
