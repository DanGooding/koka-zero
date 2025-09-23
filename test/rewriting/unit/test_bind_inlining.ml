open! Core
open Import

let%expect_test "bind inlining - simple expression" =
  let translated =
    (* f() + g() *)
    Koka_zero_evidence_translation.Private.translate_expr
      (Operator
         ( Application
             ( Value (Variable (Variable.of_user "f"))
             , []
             , Labels
                 (Effect.Label.Set.singleton (Effect.Label.of_string "ask")) )
         , Int Plus
         , Application
             ( Value (Variable (Variable.of_user "g"))
             , []
             , Labels
                 (Effect.Label.Set.singleton (Effect.Label.of_string "ask")) )
         ))
      ~evv:Nil_evidence_vector
    |> Or_static_error.ok_exn
  in
  print_s [%sexp (translated : EPS.Expr.t)];
  [%expect
    {|
    (Application (Variable (Language bind))
     (((Let (Variable (Generated mon_0)) Pure (Variable (User f))
        (Application (Variable (Generated mon_0)) ((Nil_evidence_vector Pure))
         Ctl))
       Ctl)
      (Nil_evidence_vector Pure)
      ((Lambda
        ((((Variable (Generated mon_1)) Pure)
          ((Variable (Generated mon_2)) Pure))
         Ctl
         (Application (Variable (Language bind))
          (((Let (Variable (Generated mon_3)) Pure (Variable (User g))
             (Application (Variable (Generated mon_3))
              (((Variable (Generated mon_2)) Pure)) Ctl))
            Ctl)
           ((Variable (Generated mon_2)) Pure)
           ((Lambda
             ((((Variable (Generated mon_4)) Pure)
               ((Variable (Generated mon_5)) Pure))
              Ctl
              (Construct_pure
               (Operator (Variable (Generated mon_1)) (Int Plus)
                (Variable (Generated mon_4))))))
            Pure))
          Ctl)))
       Pure))
     Ctl)
    |}];
  let rewritten, extra_decls =
    Koka_zero_evidence_rewriting.Private.apply_bind_inlining
      translated
      ~toplevel:(Variable.Set.of_list [ Primitive_names.bind ])
    |> Or_static_error.ok_exn
  in
  print_s [%message (rewritten : EPS.Expr.t)];
  [%expect
    {|
    (rewritten
     (Match_ctl
      (subject
       (Let (Variable (Generated mon_0)) Pure (Variable (User f))
        (Application (Variable (Generated mon_0)) ((Nil_evidence_vector Pure))
         Ctl)))
      (pure_branch
       ((Variable (Generated mon_1))
        (Let (Variable (Generated mon_2)) Pure Nil_evidence_vector
         (Match_ctl
          (subject
           (Let (Variable (Generated mon_3)) Pure (Variable (User g))
            (Application (Variable (Generated mon_3))
             (((Variable (Generated mon_2)) Pure)) Ctl)))
          (pure_branch
           ((Variable (Generated mon_4))
            (Let (Variable (Generated mon_5)) Pure (Variable (Generated mon_2))
             (Construct_pure
              (Operator (Variable (Generated mon_1)) (Int Plus)
               (Variable (Generated mon_4)))))))
          (yield_branch
           ((Generated opt_1) (Generated opt_2) (Generated opt_3)
            (Construct_yield (marker (Variable (Generated opt_1)))
             (op_clause (Variable (Generated opt_2)))
             (resumption
              (Lambda
               ((((Variable (Generated opt_4)) Pure)
                 ((Variable (Generated opt_5)) Pure))
                Ctl
                (Application (Variable (Language bind))
                 (((Application (Variable (Generated opt_3))
                    (((Variable (Generated opt_4)) Pure)
                     ((Variable (Generated opt_5)) Pure))
                    Ctl)
                   Ctl)
                  ((Variable (Generated opt_5)) Pure)
                  ((Application (Variable (Generated opt_0))
                    (((Variable (Generated mon_1)) Pure)) Pure)
                   Pure))
                 Ctl)))))))))))
      (yield_branch
       ((Generated opt_7) (Generated opt_8) (Generated opt_9)
        (Construct_yield (marker (Variable (Generated opt_7)))
         (op_clause (Variable (Generated opt_8)))
         (resumption
          (Lambda
           ((((Variable (Generated opt_10)) Pure)
             ((Variable (Generated opt_11)) Pure))
            Ctl
            (Application (Variable (Language bind))
             (((Application (Variable (Generated opt_9))
                (((Variable (Generated opt_10)) Pure)
                 ((Variable (Generated opt_11)) Pure))
                Ctl)
               Ctl)
              ((Variable (Generated opt_11)) Pure)
              ((Application (Variable (Generated opt_6))
                (((Variable (User g)) Pure)) Pure)
               Pure))
             Ctl)))))))))
    |}];
  print_s [%message (extra_decls : EPS.Program.Fun_decl.t list)];
  [%expect
    {|
    (extra_decls
     (((Generated opt_0)
       ((((Variable (Generated mon_1)) Pure)) Pure
        (Lambda
         ((((Variable (Generated mon_4)) Pure)
           ((Variable (Generated mon_5)) Pure))
          Ctl
          (Construct_pure
           (Operator (Variable (Generated mon_1)) (Int Plus)
            (Variable (Generated mon_4))))))))
      ((Generated opt_6)
       ((((Variable (User g)) Pure)) Pure
        (Lambda
         ((((Variable (Generated mon_1)) Pure)
           ((Variable (Generated mon_2)) Pure))
          Ctl
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_3)) Pure (Variable (User g))
              (Application (Variable (Generated mon_3))
               (((Variable (Generated mon_2)) Pure)) Ctl))
             Ctl)
            ((Variable (Generated mon_2)) Pure)
            ((Application (Variable (Generated opt_0))
              (((Variable (Generated mon_1)) Pure)) Pure)
             Pure))
           Ctl)))))))
    |}]
;;
