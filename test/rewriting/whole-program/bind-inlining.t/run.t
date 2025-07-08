Show the generated code for a program with many binds
  $ export PROJECT_ROOT=../../../..
  $ export DUMP_EPS=1

First show without any rewriting optimisations
  $ ../compile.sh many-binds.kk -dump-eps
  ((effect_declarations
    (((name console)
      (operations
       ((User print-int) (User println) (User println-int) (User read-int))))))
   (fun_declarations
    (((Language bind)
      ((((Variable (Generated mon_0)) Ctl) ((Variable (Generated mon_1)) Pure)
        ((Variable (Generated mon_2)) Pure))
       Ctl
       (Match_ctl (subject (Variable (Generated mon_0)))
        (pure_branch
         ((Generated mon_3)
          (Application (Variable (Generated mon_2))
           (((Variable (Generated mon_3)) Pure)
            ((Variable (Generated mon_1)) Pure))
           Ctl)))
        (yield_branch
         ((Generated mon_4) (Generated mon_5) (Generated mon_6)
          (Construct_yield (marker (Variable (Generated mon_4)))
           (op_clause (Variable (Generated mon_5)))
           (resumption
            (Lambda
             ((((Variable (Generated mon_7)) Pure)
               ((Variable (Generated mon_8)) Pure))
              Ctl
              (Application (Variable (Language bind))
               (((Application (Variable (Generated mon_6))
                  (((Variable (Generated mon_7)) Pure)
                   ((Variable (Generated mon_8)) Pure))
                  Ctl)
                 Ctl)
                ((Variable (Generated mon_8)) Pure)
                ((Variable (Generated mon_2)) Pure))
               Ctl))))))))))
     ((Language prompt)
      ((((Variable (Generated mon_9)) Pure)
        ((Variable (Generated mon_10)) Pure)
        ((Variable (Generated mon_11)) Pure)
        ((Variable (Generated mon_12)) Pure)
        ((Variable (Generated mon_13)) Pure))
       Ctl
       (Match_ctl
        (subject
         (Application (Variable (Generated mon_12))
          (((Cons_evidence_vector (label (Variable (Generated mon_9)))
             (marker (Variable (Generated mon_10)))
             (handler (Variable (Generated mon_11)))
             (handler_site_vector (Variable (Generated mon_13)))
             (vector_tail (Variable (Generated mon_13))))
            Pure))
          Ctl))
        (pure_branch
         ((Generated mon_14) (Construct_pure (Variable (Generated mon_14)))))
        (yield_branch
         ((Generated mon_15) (Generated mon_16) (Generated mon_17)
          (If_then_else
           (Markers_equal (Variable (Generated mon_10))
            (Variable (Generated mon_15)))
           (Application (Variable (Generated mon_16))
            (((Lambda
               ((((Variable (Generated mon_18)) Pure)
                 ((Variable (Generated mon_19)) Pure))
                Ctl
                (Application (Variable (Language prompt))
                 (((Variable (Generated mon_9)) Pure)
                  ((Variable (Generated mon_10)) Pure)
                  ((Variable (Generated mon_11)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_20)) Pure)) Ctl
                     (Application (Variable (Generated mon_17))
                      (((Variable (Generated mon_18)) Pure)
                       ((Variable (Generated mon_20)) Pure))
                      Ctl)))
                   Pure)
                  ((Variable (Generated mon_19)) Pure))
                 Ctl)))
              Pure)
             ((Variable (Generated mon_13)) Pure))
            Ctl)
           (Construct_yield (marker (Variable (Generated mon_15)))
            (op_clause (Variable (Generated mon_16)))
            (resumption
             (Lambda
              ((((Variable (Generated mon_18)) Pure)
                ((Variable (Generated mon_19)) Pure))
               Ctl
               (Application (Variable (Language prompt))
                (((Variable (Generated mon_9)) Pure)
                 ((Variable (Generated mon_10)) Pure)
                 ((Variable (Generated mon_11)) Pure)
                 ((Lambda
                   ((((Variable (Generated mon_20)) Pure)) Ctl
                    (Application (Variable (Generated mon_17))
                     (((Variable (Generated mon_18)) Pure)
                      ((Variable (Generated mon_20)) Pure))
                     Ctl)))
                  Pure)
                 ((Variable (Generated mon_19)) Pure))
                Ctl)))))))))))
     ((Language handler)
      ((((Variable (Generated mon_21)) Pure)
        ((Variable (Generated mon_22)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_23)) Pure)
          ((Variable (Generated mon_24)) Pure))
         Ctl
         (Application (Variable (Language prompt))
          (((Variable (Generated mon_21)) Pure) (Fresh_marker Pure)
           ((Variable (Generated mon_22)) Pure)
           ((Variable (Generated mon_23)) Pure)
           ((Variable (Generated mon_24)) Pure))
          Ctl)))))
     ((Language under)
      ((((Variable (Generated mon_25)) Pure)
        ((Variable (Generated mon_26)) Pure)
        ((Variable (Generated mon_27)) Pure)
        ((Variable (Generated mon_28)) Pure))
       Ctl
       (Match_ctl
        (subject
         (Application (Variable (Generated mon_28))
          (((Variable (Generated mon_27)) Pure)
           ((Variable (Generated mon_26)) Pure))
          Ctl))
        (pure_branch
         ((Generated mon_29) (Construct_pure (Variable (Generated mon_29)))))
        (yield_branch
         ((Generated mon_30) (Generated mon_31) (Generated mon_32)
          (Construct_yield (marker (Variable (Generated mon_30)))
           (op_clause (Variable (Generated mon_31)))
           (resumption
            (Lambda
             ((((Variable (Generated mon_33)) Pure)
               ((Variable (Generated mon_34)) Pure))
              Ctl
              (Application (Variable (Language under))
               (((Variable (Generated mon_25)) Pure)
                ((Get_evidence_handler_site_vector
                  (Lookup_evidence (label (Variable (Generated mon_25)))
                   (vector (Variable (Generated mon_34)))))
                 Pure)
                ((Variable (Generated mon_33)) Pure)
                ((Variable (Generated mon_32)) Pure))
               Ctl))))))))))
     ((Language perform)
      ((((Variable (Generated mon_35)) Pure)
        ((Variable (Generated mon_36)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_37)) Pure)
          ((Variable (Generated mon_38)) Pure))
         Ctl
         (Match_op
          (subject
           (Application (Variable (Generated mon_36))
            (((Get_evidence_handler
               (Lookup_evidence (label (Variable (Generated mon_35)))
                (vector (Variable (Generated mon_38)))))
              Pure))
            Pure))
          (normal_branch
           ((Generated mon_39)
            (Construct_yield
             (marker
              (Get_evidence_marker
               (Lookup_evidence (label (Variable (Generated mon_35)))
                (vector (Variable (Generated mon_38))))))
             (op_clause
              (Lambda
               ((((Variable (Generated mon_42)) Pure)
                 ((Variable (Generated mon_43)) Pure))
                Ctl
                (Application (Variable (Generated mon_39))
                 (((Variable (Generated mon_37)) Pure)
                  ((Variable (Generated mon_42)) Pure)
                  ((Variable (Generated mon_43)) Pure))
                 Ctl))))
             (resumption
              (Lambda
               ((((Variable (Generated mon_40)) Pure)
                 ((Variable (Generated mon_41)) Pure))
                Ctl (Construct_pure (Variable (Generated mon_40)))))))))
          (tail_branch
           ((Generated mon_44)
            (Application (Variable (Language under))
             (((Variable (Generated mon_35)) Pure)
              ((Get_evidence_handler_site_vector
                (Lookup_evidence (label (Variable (Generated mon_35)))
                 (vector (Variable (Generated mon_38)))))
               Pure)
              ((Variable (Generated mon_37)) Pure)
              ((Variable (Generated mon_44)) Pure))
             Ctl))))))))
     ((User foo)
      ((((Variable (User f)) Pure) ((Variable (User g)) Pure)
        ((Variable (User h)) Pure) ((Variable (User x)) Pure)
        ((Variable (Generated mon_45)) Pure))
       Ctl
       (Application (Variable (Language bind))
        (((Application (Variable (Language bind))
           (((Application (Variable (Language bind))
              (((Let (Variable (Generated mon_46)) Pure (Variable (User f))
                 (Application (Variable (Language bind))
                  (((Let (Variable (Generated mon_47)) Pure (Variable (User g))
                     (Let (Variable (Generated mon_48)) Pure
                      (Variable (User x))
                      (Application (Variable (Generated mon_47))
                       (((Variable (Generated mon_48)) Pure)
                        ((Variable (Generated mon_45)) Pure))
                       Ctl)))
                    Ctl)
                   ((Variable (Generated mon_45)) Pure)
                   ((Lambda
                     ((((Variable (Generated mon_49)) Pure)
                       ((Variable (Generated mon_50)) Pure))
                      Ctl
                      (Application (Variable (Generated mon_46))
                       (((Variable (Generated mon_49)) Pure)
                        ((Variable (Generated mon_50)) Pure))
                       Ctl)))
                    Pure))
                  Ctl))
                Ctl)
               ((Variable (Generated mon_45)) Pure)
               ((Lambda
                 ((((Variable (Generated mon_51)) Pure)
                   ((Variable (Generated mon_52)) Pure))
                  Ctl
                  (Construct_pure
                   (Let (Variable (Generated mon_53)) Pure (Literal (Int 0))
                    (Operator (Variable (Generated mon_51)) (Int Greater_than)
                     (Variable (Generated mon_53)))))))
                Pure))
              Ctl)
             Ctl)
            ((Variable (Generated mon_45)) Pure)
            ((Lambda
              ((((Variable (Generated mon_54)) Pure)
                ((Variable (Generated mon_55)) Pure))
               Ctl
               (If_then_else (Variable (Generated mon_54))
                (Application (Variable (Language bind))
                 (((Let (Variable (Generated mon_56)) Pure (Variable (User h))
                    (Let (Variable (Generated mon_57)) Pure (Literal (Int 0))
                     (Application (Variable (Generated mon_56))
                      (((Variable (Generated mon_57)) Pure)
                       ((Variable (Generated mon_55)) Pure))
                      Ctl)))
                   Ctl)
                  ((Variable (Generated mon_55)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_58)) Pure)
                      ((Variable (Generated mon_59)) Pure))
                     Ctl
                     (Let (Variable (Generated mon_60)) Pure (Literal (Int 1))
                      (Application (Variable (Generated mon_58))
                       (((Variable (Generated mon_60)) Pure)
                        ((Variable (Generated mon_59)) Pure))
                       Ctl))))
                   Pure))
                 Ctl)
                (Application (Variable (Language bind))
                 (((Let (Variable (Generated mon_61)) Pure (Variable (User h))
                    (Let (Variable (Generated mon_62)) Pure (Literal (Int 1))
                     (Application (Variable (Generated mon_61))
                      (((Variable (Generated mon_62)) Pure)
                       ((Variable (Generated mon_55)) Pure))
                      Ctl)))
                   Ctl)
                  ((Variable (Generated mon_55)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_63)) Pure)
                      ((Variable (Generated mon_64)) Pure))
                     Ctl
                     (Let (Variable (Generated mon_65)) Pure (Literal (Int 0))
                      (Application (Variable (Generated mon_63))
                       (((Variable (Generated mon_65)) Pure)
                        ((Variable (Generated mon_64)) Pure))
                       Ctl))))
                   Pure))
                 Ctl))))
             Pure))
           Ctl)
          Ctl)
         ((Variable (Generated mon_45)) Pure)
         ((Lambda
           ((((Variable (Generated mon_66)) Pure)
             ((Variable (Generated mon_67)) Pure))
            Ctl
            (Let (Variable (User z)) Pure (Variable (Generated mon_66))
             (Application (Variable (Language bind))
              (((Let (Variable (Generated mon_68)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_69)) Pure (Variable (User z))
                  (Application (Variable (Generated mon_68))
                   (((Variable (Generated mon_69)) Pure)
                    ((Variable (Generated mon_67)) Pure))
                   Ctl)))
                Ctl)
               ((Variable (Generated mon_67)) Pure)
               ((Lambda
                 ((((Variable (Generated mon_70)) Pure)
                   ((Variable (Generated mon_71)) Pure))
                  Ctl
                  (Let (Variable (Generated mon_72)) Pure (Variable (User z))
                   (Application (Variable (Generated mon_70))
                    (((Variable (Generated mon_72)) Pure)
                     ((Variable (Generated mon_71)) Pure))
                    Ctl))))
                Pure))
              Ctl))))
          Pure))
        Ctl)))
     ((User main)
      ((((Variable (Generated mon_73)) Pure)) Ctl
       (Let (Variable (Generated mon_75)) Pure
        (Application (Variable (Language perform))
         (((Effect_label console) Pure)
          ((Lambda
            ((((Variable (Generated mon_74)) Pure)) Pure
             (Select_operation console (User println-int)
              (Variable (Generated mon_74)))))
           Pure))
         Pure)
        (Let (Variable (Generated mon_92)) Pure
         (Let (Variable (Generated mon_76)) Pure (Variable (User foo))
          (Let (Variable (Generated mon_80)) Pure
           (Lambda
            ((((Variable (User y)) Pure) ((Variable (Generated mon_77)) Pure))
             Ctl
             (Construct_pure
              (Let (Variable (Generated mon_78)) Pure (Variable (User y))
               (Let (Variable (Generated mon_79)) Pure (Variable (User y))
                (Operator (Variable (Generated mon_78)) (Int Times)
                 (Variable (Generated mon_79))))))))
           (Let (Variable (Generated mon_84)) Pure
            (Lambda
             ((((Variable (User x)) Pure) ((Variable (Generated mon_81)) Pure))
              Ctl
              (Construct_pure
               (Let (Variable (Generated mon_82)) Pure (Variable (User x))
                (Let (Variable (Generated mon_83)) Pure (Variable (User x))
                 (Operator (Variable (Generated mon_82)) (Int Plus)
                  (Variable (Generated mon_83))))))))
            (Let (Variable (Generated mon_89)) Pure
             (Lambda
              ((((Variable (User m)) Pure)
                ((Variable (Generated mon_85)) Pure))
               Ctl
               (Construct_pure
                (Lambda
                 ((((Variable (User n)) Pure)
                   ((Variable (Generated mon_86)) Pure))
                  Ctl
                  (Construct_pure
                   (Let (Variable (Generated mon_87)) Pure (Variable (User m))
                    (Let (Variable (Generated mon_88)) Pure (Variable (User n))
                     (Operator (Variable (Generated mon_87)) (Int Minus)
                      (Variable (Generated mon_88)))))))))))
             (Let (Variable (Generated mon_90)) Pure (Literal (Int 3))
              (Match_ctl_pure
               (subject
                (Application (Variable (Generated mon_76))
                 (((Variable (Generated mon_80)) Pure)
                  ((Variable (Generated mon_84)) Pure)
                  ((Variable (Generated mon_89)) Pure)
                  ((Variable (Generated mon_90)) Pure)
                  ((Variable (Generated mon_73)) Pure))
                 Ctl))
               (pure_branch ((Generated mon_91) (Variable (Generated mon_91))))))))))
         (Application (Variable (Generated mon_75))
          (((Variable (Generated mon_92)) Pure)
           ((Variable (Generated mon_73)) Pure))
          Ctl)))))
     ((Language main)
      ((((Variable (Generated mon_93)) Pure)) Ctl
       (Application (Variable (Language bind))
        (((Let (Variable (Generated mon_100)) Pure
           (Application (Variable (Language handler))
            (((Effect_label console) Pure)
             ((Construct_handler (handled_effect console)
               (operation_clauses
                (((User print-int)
                  (Construct_op_tail
                   (Lambda
                    ((((Variable (Language x)) Pure)
                      ((Variable (Generated mon_94)) Pure))
                     Ctl
                     (Construct_pure
                      (Let (Variable (Generated mon_95)) Pure
                       (Variable (Language x))
                       (Impure_built_in
                        (Impure_print_int (value (Variable (Generated mon_95)))
                         (newline false)))))))))
                 ((User println)
                  (Construct_op_tail
                   (Lambda
                    (((Wildcard Pure) ((Variable (Generated mon_96)) Pure)) Ctl
                     (Construct_pure (Impure_built_in Impure_println))))))
                 ((User println-int)
                  (Construct_op_tail
                   (Lambda
                    ((((Variable (Language x)) Pure)
                      ((Variable (Generated mon_97)) Pure))
                     Ctl
                     (Construct_pure
                      (Let (Variable (Generated mon_98)) Pure
                       (Variable (Language x))
                       (Impure_built_in
                        (Impure_print_int (value (Variable (Generated mon_98)))
                         (newline true)))))))))
                 ((User read-int)
                  (Construct_op_tail
                   (Lambda
                    (((Wildcard Pure) ((Variable (Generated mon_99)) Pure)) Ctl
                     (Construct_pure (Impure_built_in Impure_read_int)))))))))
              Pure))
            Pure)
           (Let (Variable (Generated mon_101)) Pure (Variable (User main))
            (Application (Variable (Generated mon_100))
             (((Variable (Generated mon_101)) Pure)
              ((Variable (Generated mon_93)) Pure))
             Ctl)))
          Ctl)
         ((Variable (Generated mon_93)) Pure)
         ((Lambda
           ((((Variable (Generated mon_102)) Pure)
             ((Variable (Generated mon_103)) Pure))
            Ctl (Construct_pure (Literal Unit))))
          Pure))
        Ctl)))))
   (entry_expr
    (Application (Variable (Language main)) ((Nil_evidence_vector Pure)) Ctl)))

Then show with bind-inlining and other rewriting applied
  $ OPT_LEVEL=2 ../compile.sh many-binds.kk -dump-eps
  ((effect_declarations
    (((name console)
      (operations
       ((User print-int) (User println) (User println-int) (User read-int))))))
   (fun_declarations
    (((Language bind)
      ((((Variable (Generated mon_0)) Ctl) ((Variable (Generated mon_1)) Pure)
        ((Variable (Generated mon_2)) Pure))
       Ctl
       (Match_ctl (subject (Variable (Generated mon_0)))
        (pure_branch
         ((Generated mon_3)
          (Application (Variable (Generated mon_2))
           (((Variable (Generated mon_3)) Pure)
            ((Variable (Generated mon_1)) Pure))
           Ctl)))
        (yield_branch
         ((Generated mon_4) (Generated mon_5) (Generated mon_6)
          (Construct_yield (marker (Variable (Generated mon_4)))
           (op_clause (Variable (Generated mon_5)))
           (resumption
            (Lambda
             ((((Variable (Generated mon_7)) Pure)
               ((Variable (Generated mon_8)) Pure))
              Ctl
              (Application (Variable (Language bind))
               (((Application (Variable (Generated mon_6))
                  (((Variable (Generated mon_7)) Pure)
                   ((Variable (Generated mon_8)) Pure))
                  Ctl)
                 Ctl)
                ((Variable (Generated mon_8)) Pure)
                ((Variable (Generated mon_2)) Pure))
               Ctl))))))))))
     ((Language prompt)
      ((((Variable (Generated mon_9)) Pure)
        ((Variable (Generated mon_10)) Pure)
        ((Variable (Generated mon_11)) Pure)
        ((Variable (Generated mon_12)) Pure)
        ((Variable (Generated mon_13)) Pure))
       Ctl
       (Match_ctl
        (subject
         (Application (Variable (Generated mon_12))
          (((Cons_evidence_vector (label (Variable (Generated mon_9)))
             (marker (Variable (Generated mon_10)))
             (handler (Variable (Generated mon_11)))
             (handler_site_vector (Variable (Generated mon_13)))
             (vector_tail (Variable (Generated mon_13))))
            Pure))
          Ctl))
        (pure_branch
         ((Generated mon_14) (Construct_pure (Variable (Generated mon_14)))))
        (yield_branch
         ((Generated mon_15) (Generated mon_16) (Generated mon_17)
          (If_then_else
           (Markers_equal (Variable (Generated mon_10))
            (Variable (Generated mon_15)))
           (Application (Variable (Generated mon_16))
            (((Lambda
               ((((Variable (Generated mon_18)) Pure)
                 ((Variable (Generated mon_19)) Pure))
                Ctl
                (Application (Variable (Language prompt))
                 (((Variable (Generated mon_9)) Pure)
                  ((Variable (Generated mon_10)) Pure)
                  ((Variable (Generated mon_11)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_20)) Pure)) Ctl
                     (Application (Variable (Generated mon_17))
                      (((Variable (Generated mon_18)) Pure)
                       ((Variable (Generated mon_20)) Pure))
                      Ctl)))
                   Pure)
                  ((Variable (Generated mon_19)) Pure))
                 Ctl)))
              Pure)
             ((Variable (Generated mon_13)) Pure))
            Ctl)
           (Construct_yield (marker (Variable (Generated mon_15)))
            (op_clause (Variable (Generated mon_16)))
            (resumption
             (Lambda
              ((((Variable (Generated mon_18)) Pure)
                ((Variable (Generated mon_19)) Pure))
               Ctl
               (Application (Variable (Language prompt))
                (((Variable (Generated mon_9)) Pure)
                 ((Variable (Generated mon_10)) Pure)
                 ((Variable (Generated mon_11)) Pure)
                 ((Lambda
                   ((((Variable (Generated mon_20)) Pure)) Ctl
                    (Application (Variable (Generated mon_17))
                     (((Variable (Generated mon_18)) Pure)
                      ((Variable (Generated mon_20)) Pure))
                     Ctl)))
                  Pure)
                 ((Variable (Generated mon_19)) Pure))
                Ctl)))))))))))
     ((Language handler)
      ((((Variable (Generated mon_21)) Pure)
        ((Variable (Generated mon_22)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_23)) Pure)
          ((Variable (Generated mon_24)) Pure))
         Ctl
         (Application (Variable (Language prompt))
          (((Variable (Generated mon_21)) Pure) (Fresh_marker Pure)
           ((Variable (Generated mon_22)) Pure)
           ((Variable (Generated mon_23)) Pure)
           ((Variable (Generated mon_24)) Pure))
          Ctl)))))
     ((Language under)
      ((((Variable (Generated mon_25)) Pure)
        ((Variable (Generated mon_26)) Pure)
        ((Variable (Generated mon_27)) Pure)
        ((Variable (Generated mon_28)) Pure))
       Ctl
       (Match_ctl
        (subject
         (Application (Variable (Generated mon_28))
          (((Variable (Generated mon_27)) Pure)
           ((Variable (Generated mon_26)) Pure))
          Ctl))
        (pure_branch
         ((Generated mon_29) (Construct_pure (Variable (Generated mon_29)))))
        (yield_branch
         ((Generated mon_30) (Generated mon_31) (Generated mon_32)
          (Construct_yield (marker (Variable (Generated mon_30)))
           (op_clause (Variable (Generated mon_31)))
           (resumption
            (Lambda
             ((((Variable (Generated mon_33)) Pure)
               ((Variable (Generated mon_34)) Pure))
              Ctl
              (Application (Variable (Language under))
               (((Variable (Generated mon_25)) Pure)
                ((Get_evidence_handler_site_vector
                  (Lookup_evidence (label (Variable (Generated mon_25)))
                   (vector (Variable (Generated mon_34)))))
                 Pure)
                ((Variable (Generated mon_33)) Pure)
                ((Variable (Generated mon_32)) Pure))
               Ctl))))))))))
     ((Language perform)
      ((((Variable (Generated mon_35)) Pure)
        ((Variable (Generated mon_36)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_37)) Pure)
          ((Variable (Generated mon_38)) Pure))
         Ctl
         (Match_op
          (subject
           (Application (Variable (Generated mon_36))
            (((Get_evidence_handler
               (Lookup_evidence (label (Variable (Generated mon_35)))
                (vector (Variable (Generated mon_38)))))
              Pure))
            Pure))
          (normal_branch
           ((Generated mon_39)
            (Construct_yield
             (marker
              (Get_evidence_marker
               (Lookup_evidence (label (Variable (Generated mon_35)))
                (vector (Variable (Generated mon_38))))))
             (op_clause
              (Lambda
               ((((Variable (Generated mon_42)) Pure)
                 ((Variable (Generated mon_43)) Pure))
                Ctl
                (Application (Variable (Generated mon_39))
                 (((Variable (Generated mon_37)) Pure)
                  ((Variable (Generated mon_42)) Pure)
                  ((Variable (Generated mon_43)) Pure))
                 Ctl))))
             (resumption
              (Lambda
               ((((Variable (Generated mon_40)) Pure)
                 ((Variable (Generated mon_41)) Pure))
                Ctl (Construct_pure (Variable (Generated mon_40)))))))))
          (tail_branch
           ((Generated mon_44)
            (Application (Variable (Language under))
             (((Variable (Generated mon_35)) Pure)
              ((Get_evidence_handler_site_vector
                (Lookup_evidence (label (Variable (Generated mon_35)))
                 (vector (Variable (Generated mon_38)))))
               Pure)
              ((Variable (Generated mon_37)) Pure)
              ((Variable (Generated mon_44)) Pure))
             Ctl))))))))
     ((Generated opt_21)
      ((((Variable (Generated mon_46)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_49)) Pure)
          ((Variable (Generated mon_50)) Pure))
         Ctl
         (Application (Variable (Generated mon_46))
          (((Variable (Generated mon_49)) Pure)
           ((Variable (Generated mon_50)) Pure))
          Ctl)))))
     ((Generated opt_20)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_51)) Pure)
          ((Variable (Generated mon_52)) Pure))
         Ctl
         (Construct_pure
          (Let (Variable (Generated mon_53)) Pure (Literal (Int 0))
           (Operator (Variable (Generated mon_51)) (Int Greater_than)
            (Variable (Generated mon_53)))))))))
     ((Generated opt_7)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_58)) Pure)
          ((Variable (Generated mon_59)) Pure))
         Ctl
         (Let (Variable (Generated mon_60)) Pure (Literal (Int 1))
          (Application (Variable (Generated mon_58))
           (((Variable (Generated mon_60)) Pure)
            ((Variable (Generated mon_59)) Pure))
           Ctl))))))
     ((Generated opt_13)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_63)) Pure)
          ((Variable (Generated mon_64)) Pure))
         Ctl
         (Let (Variable (Generated mon_65)) Pure (Literal (Int 0))
          (Application (Variable (Generated mon_63))
           (((Variable (Generated mon_65)) Pure)
            ((Variable (Generated mon_64)) Pure))
           Ctl))))))
     ((Generated opt_19)
      ((((Variable (User h)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_54)) Pure)
          ((Variable (Generated mon_55)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_54))
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_56)) Pure (Variable (User h))
              (Let (Variable (Generated mon_57)) Pure (Literal (Int 0))
               (Application (Variable (Generated mon_56))
                (((Variable (Generated mon_57)) Pure)
                 ((Variable (Generated mon_55)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_55)) Pure)
            ((Application (Variable (Generated opt_7)) () Pure) Pure))
           Ctl)
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_61)) Pure (Variable (User h))
              (Let (Variable (Generated mon_62)) Pure (Literal (Int 1))
               (Application (Variable (Generated mon_61))
                (((Variable (Generated mon_62)) Pure)
                 ((Variable (Generated mon_55)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_55)) Pure)
            ((Application (Variable (Generated opt_13)) () Pure) Pure))
           Ctl))))))
     ((Generated opt_0)
      ((((Variable (User z)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_70)) Pure)
          ((Variable (Generated mon_71)) Pure))
         Ctl
         (Let (Variable (Generated mon_72)) Pure (Variable (User z))
          (Application (Variable (Generated mon_70))
           (((Variable (Generated mon_72)) Pure)
            ((Variable (Generated mon_71)) Pure))
           Ctl))))))
     ((Generated opt_6)
      ((((Variable (User h)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_66)) Pure)
          ((Variable (Generated mon_67)) Pure))
         Ctl
         (Let (Variable (User z)) Pure (Variable (Generated mon_66))
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_68)) Pure (Variable (User h))
              (Let (Variable (Generated mon_69)) Pure (Variable (User z))
               (Application (Variable (Generated mon_68))
                (((Variable (Generated mon_69)) Pure)
                 ((Variable (Generated mon_67)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_67)) Pure)
            ((Application (Variable (Generated opt_0))
              (((Variable (User z)) Pure)) Pure)
             Pure))
           Ctl))))))
     ((User foo)
      ((((Variable (User f)) Pure) ((Variable (User g)) Pure)
        ((Variable (User h)) Pure) ((Variable (User x)) Pure)
        ((Variable (Generated mon_45)) Pure))
       Ctl
       (Match_ctl
        (subject
         (Match_ctl
          (subject
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_46)) Pure (Variable (User f))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_47)) Pure (Variable (User g))
                 (Let (Variable (Generated mon_48)) Pure (Variable (User x))
                  (Application (Variable (Generated mon_47))
                   (((Variable (Generated mon_48)) Pure)
                    ((Variable (Generated mon_45)) Pure))
                   Ctl))))
               (pure_branch
                ((Generated mon_49)
                 (Let (Variable (Generated mon_50)) Pure
                  (Variable (Generated mon_45))
                  (Application (Variable (Generated mon_46))
                   (((Variable (Generated mon_49)) Pure)
                    ((Variable (Generated mon_50)) Pure))
                   Ctl))))
               (yield_branch
                ((Generated opt_22) (Generated opt_23) (Generated opt_24)
                 (Construct_yield (marker (Variable (Generated opt_22)))
                  (op_clause (Variable (Generated opt_23)))
                  (resumption
                   (Lambda
                    ((((Variable (Generated opt_25)) Pure)
                      ((Variable (Generated opt_26)) Pure))
                     Ctl
                     (Application (Variable (Language bind))
                      (((Application (Variable (Generated opt_24))
                         (((Variable (Generated opt_25)) Pure)
                          ((Variable (Generated opt_26)) Pure))
                         Ctl)
                        Ctl)
                       ((Variable (Generated opt_26)) Pure)
                       ((Application (Variable (Generated opt_21))
                         (((Variable (Generated mon_46)) Pure)) Pure)
                        Pure))
                      Ctl))))))))))
            (pure_branch
             ((Generated mon_51)
              (Let (Variable (Generated mon_52)) Pure
               (Variable (Generated mon_45))
               (Construct_pure
                (Let (Variable (Generated mon_53)) Pure (Literal (Int 0))
                 (Operator (Variable (Generated mon_51)) (Int Greater_than)
                  (Variable (Generated mon_53))))))))
            (yield_branch
             ((Generated opt_27) (Generated opt_28) (Generated opt_29)
              (Construct_yield (marker (Variable (Generated opt_27)))
               (op_clause (Variable (Generated opt_28)))
               (resumption
                (Lambda
                 ((((Variable (Generated opt_30)) Pure)
                   ((Variable (Generated opt_31)) Pure))
                  Ctl
                  (Application (Variable (Language bind))
                   (((Application (Variable (Generated opt_29))
                      (((Variable (Generated opt_30)) Pure)
                       ((Variable (Generated opt_31)) Pure))
                      Ctl)
                     Ctl)
                    ((Variable (Generated opt_31)) Pure)
                    ((Application (Variable (Generated opt_20)) () Pure) Pure))
                   Ctl)))))))))
          (pure_branch
           ((Generated mon_54)
            (Let (Variable (Generated mon_55)) Pure
             (Variable (Generated mon_45))
             (If_then_else (Variable (Generated mon_54))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_56)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_57)) Pure (Literal (Int 0))
                  (Application (Variable (Generated mon_56))
                   (((Variable (Generated mon_57)) Pure)
                    ((Variable (Generated mon_55)) Pure))
                   Ctl))))
               (pure_branch
                ((Generated mon_58)
                 (Let (Variable (Generated mon_59)) Pure
                  (Variable (Generated mon_55))
                  (Let (Variable (Generated mon_60)) Pure (Literal (Int 1))
                   (Application (Variable (Generated mon_58))
                    (((Variable (Generated mon_60)) Pure)
                     ((Variable (Generated mon_59)) Pure))
                    Ctl)))))
               (yield_branch
                ((Generated opt_8) (Generated opt_9) (Generated opt_10)
                 (Construct_yield (marker (Variable (Generated opt_8)))
                  (op_clause (Variable (Generated opt_9)))
                  (resumption
                   (Lambda
                    ((((Variable (Generated opt_11)) Pure)
                      ((Variable (Generated opt_12)) Pure))
                     Ctl
                     (Application (Variable (Language bind))
                      (((Application (Variable (Generated opt_10))
                         (((Variable (Generated opt_11)) Pure)
                          ((Variable (Generated opt_12)) Pure))
                         Ctl)
                        Ctl)
                       ((Variable (Generated opt_12)) Pure)
                       ((Application (Variable (Generated opt_7)) () Pure)
                        Pure))
                      Ctl))))))))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_61)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_62)) Pure (Literal (Int 1))
                  (Application (Variable (Generated mon_61))
                   (((Variable (Generated mon_62)) Pure)
                    ((Variable (Generated mon_55)) Pure))
                   Ctl))))
               (pure_branch
                ((Generated mon_63)
                 (Let (Variable (Generated mon_64)) Pure
                  (Variable (Generated mon_55))
                  (Let (Variable (Generated mon_65)) Pure (Literal (Int 0))
                   (Application (Variable (Generated mon_63))
                    (((Variable (Generated mon_65)) Pure)
                     ((Variable (Generated mon_64)) Pure))
                    Ctl)))))
               (yield_branch
                ((Generated opt_14) (Generated opt_15) (Generated opt_16)
                 (Construct_yield (marker (Variable (Generated opt_14)))
                  (op_clause (Variable (Generated opt_15)))
                  (resumption
                   (Lambda
                    ((((Variable (Generated opt_17)) Pure)
                      ((Variable (Generated opt_18)) Pure))
                     Ctl
                     (Application (Variable (Language bind))
                      (((Application (Variable (Generated opt_16))
                         (((Variable (Generated opt_17)) Pure)
                          ((Variable (Generated opt_18)) Pure))
                         Ctl)
                        Ctl)
                       ((Variable (Generated opt_18)) Pure)
                       ((Application (Variable (Generated opt_13)) () Pure)
                        Pure))
                      Ctl))))))))))))
          (yield_branch
           ((Generated opt_32) (Generated opt_33) (Generated opt_34)
            (Construct_yield (marker (Variable (Generated opt_32)))
             (op_clause (Variable (Generated opt_33)))
             (resumption
              (Lambda
               ((((Variable (Generated opt_35)) Pure)
                 ((Variable (Generated opt_36)) Pure))
                Ctl
                (Application (Variable (Language bind))
                 (((Application (Variable (Generated opt_34))
                    (((Variable (Generated opt_35)) Pure)
                     ((Variable (Generated opt_36)) Pure))
                    Ctl)
                   Ctl)
                  ((Variable (Generated opt_36)) Pure)
                  ((Application (Variable (Generated opt_19))
                    (((Variable (User h)) Pure)) Pure)
                   Pure))
                 Ctl)))))))))
        (pure_branch
         ((Generated mon_66)
          (Let (Variable (Generated mon_67)) Pure (Variable (Generated mon_45))
           (Let (Variable (User z)) Pure (Variable (Generated mon_66))
            (Match_ctl
             (subject
              (Let (Variable (Generated mon_68)) Pure (Variable (User h))
               (Let (Variable (Generated mon_69)) Pure (Variable (User z))
                (Application (Variable (Generated mon_68))
                 (((Variable (Generated mon_69)) Pure)
                  ((Variable (Generated mon_67)) Pure))
                 Ctl))))
             (pure_branch
              ((Generated mon_70)
               (Let (Variable (Generated mon_71)) Pure
                (Variable (Generated mon_67))
                (Let (Variable (Generated mon_72)) Pure (Variable (User z))
                 (Application (Variable (Generated mon_70))
                  (((Variable (Generated mon_72)) Pure)
                   ((Variable (Generated mon_71)) Pure))
                  Ctl)))))
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
                       (((Variable (User z)) Pure)) Pure)
                      Pure))
                    Ctl))))))))))))
        (yield_branch
         ((Generated opt_37) (Generated opt_38) (Generated opt_39)
          (Construct_yield (marker (Variable (Generated opt_37)))
           (op_clause (Variable (Generated opt_38)))
           (resumption
            (Lambda
             ((((Variable (Generated opt_40)) Pure)
               ((Variable (Generated opt_41)) Pure))
              Ctl
              (Application (Variable (Language bind))
               (((Application (Variable (Generated opt_39))
                  (((Variable (Generated opt_40)) Pure)
                   ((Variable (Generated opt_41)) Pure))
                  Ctl)
                 Ctl)
                ((Variable (Generated opt_41)) Pure)
                ((Application (Variable (Generated opt_6))
                  (((Variable (User h)) Pure)) Pure)
                 Pure))
               Ctl))))))))))
     ((User main)
      ((((Variable (Generated mon_73)) Pure)) Ctl
       (Let (Variable (Generated mon_75)) Pure
        (Application (Variable (Language perform))
         (((Effect_label console) Pure)
          ((Lambda
            ((((Variable (Generated mon_74)) Pure)) Pure
             (Select_operation console (User println-int)
              (Variable (Generated mon_74)))))
           Pure))
         Pure)
        (Let (Variable (Generated mon_92)) Pure
         (Let (Variable (Generated mon_76)) Pure (Variable (User foo))
          (Let (Variable (Generated mon_80)) Pure
           (Lambda
            ((((Variable (User y)) Pure) ((Variable (Generated mon_77)) Pure))
             Ctl
             (Construct_pure
              (Let (Variable (Generated mon_78)) Pure (Variable (User y))
               (Let (Variable (Generated mon_79)) Pure (Variable (User y))
                (Operator (Variable (Generated mon_78)) (Int Times)
                 (Variable (Generated mon_79))))))))
           (Let (Variable (Generated mon_84)) Pure
            (Lambda
             ((((Variable (User x)) Pure) ((Variable (Generated mon_81)) Pure))
              Ctl
              (Construct_pure
               (Let (Variable (Generated mon_82)) Pure (Variable (User x))
                (Let (Variable (Generated mon_83)) Pure (Variable (User x))
                 (Operator (Variable (Generated mon_82)) (Int Plus)
                  (Variable (Generated mon_83))))))))
            (Let (Variable (Generated mon_89)) Pure
             (Lambda
              ((((Variable (User m)) Pure)
                ((Variable (Generated mon_85)) Pure))
               Ctl
               (Construct_pure
                (Lambda
                 ((((Variable (User n)) Pure)
                   ((Variable (Generated mon_86)) Pure))
                  Ctl
                  (Construct_pure
                   (Let (Variable (Generated mon_87)) Pure (Variable (User m))
                    (Let (Variable (Generated mon_88)) Pure (Variable (User n))
                     (Operator (Variable (Generated mon_87)) (Int Minus)
                      (Variable (Generated mon_88)))))))))))
             (Let (Variable (Generated mon_90)) Pure (Literal (Int 3))
              (Match_ctl_pure
               (subject
                (Application (Variable (Generated mon_76))
                 (((Variable (Generated mon_80)) Pure)
                  ((Variable (Generated mon_84)) Pure)
                  ((Variable (Generated mon_89)) Pure)
                  ((Variable (Generated mon_90)) Pure)
                  ((Variable (Generated mon_73)) Pure))
                 Ctl))
               (pure_branch ((Generated mon_91) (Variable (Generated mon_91))))))))))
         (Application (Variable (Generated mon_75))
          (((Variable (Generated mon_92)) Pure)
           ((Variable (Generated mon_73)) Pure))
          Ctl)))))
     ((Generated opt_42)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_102)) Pure)
          ((Variable (Generated mon_103)) Pure))
         Ctl (Construct_pure (Literal Unit))))))
     ((Language main)
      ((((Variable (Generated mon_93)) Pure)) Ctl
       (Match_ctl
        (subject
         (Let (Variable (Generated mon_100)) Pure
          (Application (Variable (Language handler))
           (((Effect_label console) Pure)
            ((Construct_handler (handled_effect console)
              (operation_clauses
               (((User print-int)
                 (Construct_op_tail
                  (Lambda
                   ((((Variable (Language x)) Pure)
                     ((Variable (Generated mon_94)) Pure))
                    Ctl
                    (Construct_pure
                     (Let (Variable (Generated mon_95)) Pure
                      (Variable (Language x))
                      (Impure_built_in
                       (Impure_print_int (value (Variable (Generated mon_95)))
                        (newline false)))))))))
                ((User println)
                 (Construct_op_tail
                  (Lambda
                   (((Wildcard Pure) ((Variable (Generated mon_96)) Pure)) Ctl
                    (Construct_pure (Impure_built_in Impure_println))))))
                ((User println-int)
                 (Construct_op_tail
                  (Lambda
                   ((((Variable (Language x)) Pure)
                     ((Variable (Generated mon_97)) Pure))
                    Ctl
                    (Construct_pure
                     (Let (Variable (Generated mon_98)) Pure
                      (Variable (Language x))
                      (Impure_built_in
                       (Impure_print_int (value (Variable (Generated mon_98)))
                        (newline true)))))))))
                ((User read-int)
                 (Construct_op_tail
                  (Lambda
                   (((Wildcard Pure) ((Variable (Generated mon_99)) Pure)) Ctl
                    (Construct_pure (Impure_built_in Impure_read_int)))))))))
             Pure))
           Pure)
          (Let (Variable (Generated mon_101)) Pure (Variable (User main))
           (Application (Variable (Generated mon_100))
            (((Variable (Generated mon_101)) Pure)
             ((Variable (Generated mon_93)) Pure))
            Ctl))))
        (pure_branch
         ((Generated mon_102)
          (Let (Variable (Generated mon_103)) Pure
           (Variable (Generated mon_93)) (Construct_pure (Literal Unit)))))
        (yield_branch
         ((Generated opt_43) (Generated opt_44) (Generated opt_45)
          (Construct_yield (marker (Variable (Generated opt_43)))
           (op_clause (Variable (Generated opt_44)))
           (resumption
            (Lambda
             ((((Variable (Generated opt_46)) Pure)
               ((Variable (Generated opt_47)) Pure))
              Ctl
              (Application (Variable (Language bind))
               (((Application (Variable (Generated opt_45))
                  (((Variable (Generated opt_46)) Pure)
                   ((Variable (Generated opt_47)) Pure))
                  Ctl)
                 Ctl)
                ((Variable (Generated opt_47)) Pure)
                ((Application (Variable (Generated opt_42)) () Pure) Pure))
               Ctl))))))))))))
   (entry_expr
    (Application (Variable (Language main)) ((Nil_evidence_vector Pure)) Ctl)))
