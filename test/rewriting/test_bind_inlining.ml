open! Core
open Import

let%expect_test "bind inlining" =
  let translated =
    Koka_zero_evidence_translation.Private.translate_expr
      (Operator
         ( Application (Value (Variable (Variable.of_user "f")), [])
         , Int Plus
         , Application (Value (Variable (Variable.of_user "g")), []) ))
      ~evv:Nil_evidence_vector
    |> Or_static_error.ok_exn
  in
  print_s [%sexp (translated : EPS.Expr.t)];
  [%expect
    {|
    (Application (Variable (Language bind))
     ((Let (Variable (Generated mon_0)) (Variable (User f))
       (Application (Variable (Generated mon_0)) (Nil_evidence_vector)))
      Nil_evidence_vector
      (Lambda
       (((Variable (Generated mon_1)) (Variable (Generated mon_2)))
        (Application (Variable (Language bind))
         ((Let (Variable (Generated mon_3)) (Variable (User g))
           (Application (Variable (Generated mon_3))
            ((Variable (Generated mon_2)))))
          (Variable (Generated mon_2))
          (Lambda
           (((Variable (Generated mon_4)) (Variable (Generated mon_5)))
            (Construct_pure
             (Operator (Variable (Generated mon_1)) (Int Plus)
              (Variable (Generated mon_4))))))))))))
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
     (Match_ctl
      (subject
       (Let (Variable (Generated mon_0)) (Variable (User f))
        (Application (Variable (Generated mon_0)) (Nil_evidence_vector))))
      (pure_branch
       ((Generated mon_1)
        (Let (Variable (Generated mon_2)) Nil_evidence_vector
         (Match_ctl
          (subject
           (Let (Variable (Generated mon_3)) (Variable (User g))
            (Application (Variable (Generated mon_3))
             ((Variable (Generated mon_2))))))
          (pure_branch
           ((Generated mon_4)
            (Let (Variable (Generated mon_5)) (Variable (Generated mon_2))
             (Construct_pure
              (Operator (Variable (Generated mon_1)) (Int Plus)
               (Variable (Generated mon_4)))))))
          (yield_branch
           ((Generated opt_1) (Generated opt_2) (Generated opt_3)
            (Construct_yield (marker (Variable (Generated opt_1)))
             (op_clause (Variable (Generated opt_2)))
             (resumption
              (Lambda
               (((Variable (Generated opt_4)) (Variable (Generated opt_5)))
                (Application (Variable (Language bind))
                 ((Application (Variable (Generated opt_3))
                   ((Variable (Generated opt_4)) (Variable (Generated opt_5))))
                  (Variable (Generated opt_5))
                  (Application (Variable (Generated opt_0))
                   ((Variable (Generated mon_1))))))))))))))))
      (yield_branch
       ((Generated opt_7) (Generated opt_8) (Generated opt_9)
        (Construct_yield (marker (Variable (Generated opt_7)))
         (op_clause (Variable (Generated opt_8)))
         (resumption
          (Lambda
           (((Variable (Generated opt_10)) (Variable (Generated opt_11)))
            (Application (Variable (Language bind))
             ((Application (Variable (Generated opt_9))
               ((Variable (Generated opt_10)) (Variable (Generated opt_11))))
              (Variable (Generated opt_11))
              (Application (Variable (Generated opt_6)) ((Variable (User g))))))))))))))
    |}];
  print_s [%message (extra_decls : EPS.Program.Fun_decl.t list)];
  [%expect
    {|
    (extra_decls
     (((Generated opt_0)
       (((Variable (Generated mon_1)))
        (Lambda
         (((Variable (Generated mon_4)) (Variable (Generated mon_5)))
          (Construct_pure
           (Operator (Variable (Generated mon_1)) (Int Plus)
            (Variable (Generated mon_4))))))))
      ((Generated opt_6)
       (((Variable (User g)))
        (Lambda
         (((Variable (Generated mon_1)) (Variable (Generated mon_2)))
          (Application (Variable (Language bind))
           ((Let (Variable (Generated mon_3)) (Variable (User g))
             (Application (Variable (Generated mon_3))
              ((Variable (Generated mon_2)))))
            (Variable (Generated mon_2))
            (Application (Variable (Generated opt_0))
             ((Variable (Generated mon_1))))))))))))
    |}]
;;
