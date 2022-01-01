open Core

(* TODO: should variable be a variant to allow generation of fresh ones? *)
module Variable : Identifiable.S = String
module Literal = Koka_zero_inference.Minimal_syntax.Literal
module Operator = Koka_zero_inference.Minimal_syntax.Operator

(* TODO: do I want types? definitely want effects - for evidence vector
   lookup *)

module Marker = struct
  type t = int [@@deriving sexp]
end

module Expr = struct
  type t =
    | Variable of Variable.t
    (* | Let, or convert to lamdas somehow? *)
    | Lambda of Variable.t list * t
    (* TODO: no translation is given for fix - look at what the real compiler does *)
    (* | Fix of Variable.t * t *)
    | Application of t list * t
    | Literal of Literal.t
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of t * Operator.Unary.t
    (* handler is just a function (?) *)
    (* | Handler of Label.t * handler *  *)
    (* | Perform of Label.t * t *)
    (* TODO: these are all internal... *)
    (* TODO: this is just an AST, these are just type constructors *)
    (* TODO: need pattern matching on these *)
    | Construct_pure of t
    | Construct_yield of
        { marker : t
        ; handler_clause : t
        ; resumption : t
        }
    | Match_ctl of
        { subject : t
        ; pure_branch : t (* gets one argument *)
        ; yield_branch : t (* gets one argument per field in Yield *)
        }
    (* TODO: bind isn't a primitive? *)
    | Bind of t * t
    (* TODO: note evidence vectors are not first class - can have more sensible
       primitives if desired *)
    (* honestly here is a place where GADTs would help - or at least just
       hanging on to type annotations... - what about a GADT just for the sort
       of thing: evidence vector / label / marker / handler / koka-value

       but surely there is a slippery slope where we need effects, koka
       functions, polymorphism etc... *)
    (* evaluates to a new unique marker *)
    | Fresh_marker
    | Equal_markers of t * t
    | Cons_evidence_vector of
        { label : t
        ; marker : t
        ; handler : t
              (* seems to only be for tail resumptions *)
              (* ; old_evidence : t *)
        ; vector_tail : t
        }
    | Lookup_evidence of
        { label : t
        ; vector : t
        }
    | Get_evidence_marker : t
    | Get_evidence_handler : t
    (* |  *)
    (* primitive to get an operation from a handler's runtime representation *)
    | Select_operation of int * t
  (* | *)
  [@@deriving sexp]

  (* TODO: these names mustn't clash with operations (?) *)
  let v = Variable.of_string

  (* TODO: keeping track of which arguments are curried is now hugely error
     prone and GADTs would be massively helpful! *)
  let handler v_prompt =
    let v_label = v "l" in
    let v_handler = v "h" in
    let v_action = v "f" in
    Lambda
      ( [ v_label; v_handler ]
      , Lambda
          ( v_action
          , Application (Variable v_prompt, [ Variable v_label; Fresh_marker ])
          ) )
  ;;

  let composed g f =
    (* TODO: definitely need unique names *)
    let v_arg = v "x" in
    Lambda ([ v_arg ], Application (g, Application (f, Variable v_arg)))
  ;;

  (* technically these could be primitives and be implemented in C *)
  let prompt v_prompt =
    (* TODO: generate all of these to be safely unique *)
    let v_label = v "l" in
    let v_marker = v "m" in
    let v_handler = v "h" in
    let v_e = v "e" in
    let v_vector = v "w" in
    let extended_vector =
      Cons_evidence_vector
        { label = Variable v_label
        ; marker = Variable v_marker
        ; handler = Variable v_handler
        ; vector_tail = Variable v_vector
        }
    in
    let run_in_extended_context =
      Application (Variable v_e, [ extended_vector ])
    in
    let pure_branch =
      let v_x = v "x" in
      Lambda ([ v_x ], Construct_pure (Variable v_x))
    in
    let yield_branch =
      let v_marker' = v "m2" in
      let v_handler_clause = v "f" in
      let v_resumption = v "k" in
      let resume_under =
        Application
          ( Application
              ( Application (Variable v_prompt, Variable v_label)
              , Variable v_marker )
          , Variable v_handler )
      in
      let bubble_further =
        Construct_yield
          { marker = Variable v_marker'
          ; handler_clause = Variable v_handler_clause
          ; resumption = resume_under
          }
      in
      let handle_here =
        composed
          (Application
             ( Application (Variable v_handler_clause, resume_under)
             , Variable v_vector ))
          (Variable v_resumption)
      in
      Lambda
        ( [ v_marker'; v_handler_clause; v_resumption ]
        , If_then_else
            ( Equal_markers (Variable v_marker, Variable v_marker')
            , handle_here
            , bubble_further ) )
    in
    Lambda
      ( [ v_label; v_marker; v_handler ]
      , Lambda
          ( v_e
          , Lambda
              ( v_vector
              , Match_ctl
                  { subject = run_in_extended_context
                  ; pure_branch
                  ; yield_branch
                  } ) ) )
  ;;
end
