open! Core
open Import

let%expect_test "bind inlining" =
  (let translated =
     Koka_zero_evidence_translation.Private.translate_expr
       (Operator (Value (Literal (Int 2)), Int Plus, Value (Literal (Int 3))))
       ~evv:Nil_evidence_vector
     |> Or_static_error.ok_exn
   in
   print_s [%sexp (translated : EPS.Expr.t)];
   [%expect
     {|
       (Application (Variable (Language bind))
        ((Construct_pure (Literal (Int 2))) Nil_evidence_vector
         (Lambda
          (((Variable (Generated mon_0)) (Variable (Generated mon_1)))
           (Application (Variable (Language bind))
            ((Construct_pure (Literal (Int 3))) (Variable (Generated mon_1))
             (Lambda
              (((Variable (Generated mon_2)) (Variable (Generated mon_3)))
               (Construct_pure
                (Operator (Variable (Generated mon_0)) (Int Plus)
                 (Variable (Generated mon_2))))))))))))
       |}];
   let rewritten, extra_decls =
     Koka_zero_evidence_translation.Private.Rewriting.apply_bind_inlining
       translated
       ~toplevel:(Variable.Set.of_list [ Primitives.Names.bind ])
     |> Or_static_error.ok_exn
   in
   print_s [%message (rewritten : EPS.Expr.t)];
   [%expect
     {|
       (rewritten
        (Match_ctl (subject (Construct_pure (Literal (Int 2))))
         (pure_branch
          ((Generated mon_0)
           (Let (Variable (Generated mon_1)) Nil_evidence_vector
            (Match_ctl (subject (Construct_pure (Literal (Int 3))))
             (pure_branch
              ((Generated mon_2)
               (Let (Variable (Generated mon_3)) (Variable (Generated mon_1))
                (Construct_pure
                 (Operator (Variable (Generated mon_0)) (Int Plus)
                  (Variable (Generated mon_2)))))))
             (yield_branch
              ((Generated mon_5) (Generated mon_6) (Generated mon_7)
               (Construct_yield (marker (Variable (Generated mon_5)))
                (op_clause (Variable (Generated mon_6)))
                (resumption
                 (Lambda
                  (((Variable (Generated mon_8)) (Variable (Generated mon_9)))
                   (Application (Variable (Language bind))
                    ((Application (Variable (Generated mon_7))
                      ((Variable (Generated mon_8)) (Variable (Generated mon_9))))
                     (Variable (Generated mon_9))
                     (Application (Variable (Generated mon_4))
                      ((Variable (Generated mon_0))))))))))))))))
         (yield_branch
          ((Generated mon_11) (Generated mon_12) (Generated mon_13)
           (Construct_yield (marker (Variable (Generated mon_11)))
            (op_clause (Variable (Generated mon_12)))
            (resumption
             (Lambda
              (((Variable (Generated mon_14)) (Variable (Generated mon_15)))
               (Application (Variable (Language bind))
                ((Application (Variable (Generated mon_13))
                  ((Variable (Generated mon_14)) (Variable (Generated mon_15))))
                 (Variable (Generated mon_15))
                 (Application (Variable (Generated mon_10)) ())))))))))))
       |}];
   print_s [%message (extra_decls : EPS.Program.Fun_decl.t list)])
  |> ignore;
  [%expect
    {|
    (extra_decls
     (((Generated mon_4)
       (((Variable (Generated mon_0)))
        (Lambda
         (((Variable (Generated mon_2)) (Variable (Generated mon_3)))
          (Construct_pure
           (Operator (Variable (Generated mon_0)) (Int Plus)
            (Variable (Generated mon_2))))))))
      ((Generated mon_10)
       (()
        (Lambda
         (((Variable (Generated mon_0)) (Variable (Generated mon_1)))
          (Application (Variable (Language bind))
           ((Construct_pure (Literal (Int 3))) (Variable (Generated mon_1))
            (Application (Variable (Generated mon_4))
             ((Variable (Generated mon_0))))))))))))
    |}]
;;
