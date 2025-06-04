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
      (((Variable (Generated mon_0)) (Variable (Generated mon_1))
        (Variable (Generated mon_2)))
       (Match_ctl (subject (Variable (Generated mon_0)))
        (pure_branch
         ((Generated mon_3)
          (Application (Variable (Generated mon_2))
           ((Variable (Generated mon_3)) (Variable (Generated mon_1))))))
        (yield_branch
         ((Generated mon_4) (Generated mon_5) (Generated mon_6)
          (Construct_yield (marker (Variable (Generated mon_4)))
           (op_clause (Variable (Generated mon_5)))
           (resumption
            (Lambda
             (((Variable (Generated mon_7)) (Variable (Generated mon_8)))
              (Application (Variable (Language bind))
               ((Application (Variable (Generated mon_6))
                 ((Variable (Generated mon_7)) (Variable (Generated mon_8))))
                (Variable (Generated mon_8)) (Variable (Generated mon_2)))))))))))))
     ((Language prompt)
      (((Variable (Generated mon_9)) (Variable (Generated mon_10))
        (Variable (Generated mon_11)) (Variable (Generated mon_12))
        (Variable (Generated mon_13)))
       (Match_ctl
        (subject
         (Application (Variable (Generated mon_12))
          ((Cons_evidence_vector (label (Variable (Generated mon_9)))
            (marker (Variable (Generated mon_10)))
            (handler (Variable (Generated mon_11)))
            (handler_site_vector (Variable (Generated mon_13)))
            (vector_tail (Variable (Generated mon_13)))))))
        (pure_branch
         ((Generated mon_14) (Construct_pure (Variable (Generated mon_14)))))
        (yield_branch
         ((Generated mon_15) (Generated mon_16) (Generated mon_17)
          (If_then_else
           (Markers_equal (Variable (Generated mon_10))
            (Variable (Generated mon_15)))
           (Application (Variable (Generated mon_16))
            ((Lambda
              (((Variable (Generated mon_18)) (Variable (Generated mon_19)))
               (Application (Variable (Language prompt))
                ((Variable (Generated mon_9)) (Variable (Generated mon_10))
                 (Variable (Generated mon_11))
                 (Lambda
                  (((Variable (Generated mon_20)))
                   (Application (Variable (Generated mon_17))
                    ((Variable (Generated mon_18))
                     (Variable (Generated mon_20))))))
                 (Variable (Generated mon_19))))))
             (Variable (Generated mon_13))))
           (Construct_yield (marker (Variable (Generated mon_15)))
            (op_clause (Variable (Generated mon_16)))
            (resumption
             (Lambda
              (((Variable (Generated mon_18)) (Variable (Generated mon_19)))
               (Application (Variable (Language prompt))
                ((Variable (Generated mon_9)) (Variable (Generated mon_10))
                 (Variable (Generated mon_11))
                 (Lambda
                  (((Variable (Generated mon_20)))
                   (Application (Variable (Generated mon_17))
                    ((Variable (Generated mon_18))
                     (Variable (Generated mon_20))))))
                 (Variable (Generated mon_19))))))))))))))
     ((Language handler)
      (((Variable (Generated mon_21)) (Variable (Generated mon_22)))
       (Lambda
        (((Variable (Generated mon_23)) (Variable (Generated mon_24)))
         (Application (Variable (Language prompt))
          ((Variable (Generated mon_21)) Fresh_marker
           (Variable (Generated mon_22)) (Variable (Generated mon_23))
           (Variable (Generated mon_24))))))))
     ((Language under)
      (((Variable (Generated mon_25)) (Variable (Generated mon_26))
        (Variable (Generated mon_27)) (Variable (Generated mon_28)))
       (Match_ctl
        (subject
         (Application (Variable (Generated mon_28))
          ((Variable (Generated mon_27)) (Variable (Generated mon_26)))))
        (pure_branch
         ((Generated mon_29) (Construct_pure (Variable (Generated mon_29)))))
        (yield_branch
         ((Generated mon_30) (Generated mon_31) (Generated mon_32)
          (Construct_yield (marker (Variable (Generated mon_30)))
           (op_clause (Variable (Generated mon_31)))
           (resumption
            (Lambda
             (((Variable (Generated mon_33)) (Variable (Generated mon_34)))
              (Application (Variable (Language under))
               ((Variable (Generated mon_25))
                (Get_evidence_handler_site_vector
                 (Lookup_evidence (label (Variable (Generated mon_25)))
                  (vector (Variable (Generated mon_34)))))
                (Variable (Generated mon_33)) (Variable (Generated mon_32)))))))))))))
     ((Language perform)
      (((Variable (Generated mon_35)) (Variable (Generated mon_36)))
       (Lambda
        (((Variable (Generated mon_37)) (Variable (Generated mon_38)))
         (Match_op
          (subject
           (Application (Variable (Generated mon_36))
            ((Get_evidence_handler
              (Lookup_evidence (label (Variable (Generated mon_35)))
               (vector (Variable (Generated mon_38))))))))
          (normal_branch
           ((Generated mon_39)
            (Construct_yield
             (marker
              (Get_evidence_marker
               (Lookup_evidence (label (Variable (Generated mon_35)))
                (vector (Variable (Generated mon_38))))))
             (op_clause
              (Lambda
               (((Variable (Generated mon_42)) (Variable (Generated mon_43)))
                (Application (Variable (Generated mon_39))
                 ((Variable (Generated mon_37)) (Variable (Generated mon_42))
                  (Variable (Generated mon_43)))))))
             (resumption
              (Lambda
               (((Variable (Generated mon_40)) (Variable (Generated mon_41)))
                (Construct_pure (Variable (Generated mon_40)))))))))
          (tail_branch
           ((Generated mon_44)
            (Application (Variable (Language under))
             ((Variable (Generated mon_35))
              (Get_evidence_handler_site_vector
               (Lookup_evidence (label (Variable (Generated mon_35)))
                (vector (Variable (Generated mon_38)))))
              (Variable (Generated mon_37)) (Variable (Generated mon_44)))))))))))
     ((User foo)
      (((Variable (User f)) (Variable (User g)) (Variable (User h))
        (Variable (User x)) (Variable (Generated mon_45)))
       (Application (Variable (Language bind))
        ((Application (Variable (Language bind))
          ((Application (Variable (Language bind))
            ((Let (Variable (Generated mon_46)) (Variable (User f))
              (Application (Variable (Language bind))
               ((Let (Variable (Generated mon_47)) (Variable (User g))
                 (Let (Variable (Generated mon_48)) (Variable (User x))
                  (Application (Variable (Generated mon_47))
                   ((Variable (Generated mon_48))
                    (Variable (Generated mon_45))))))
                (Variable (Generated mon_45))
                (Lambda
                 (((Variable (Generated mon_49)) (Variable (Generated mon_50)))
                  (Application (Variable (Generated mon_46))
                   ((Variable (Generated mon_49))
                    (Variable (Generated mon_50)))))))))
             (Variable (Generated mon_45))
             (Lambda
              (((Variable (Generated mon_51)) (Variable (Generated mon_52)))
               (Construct_pure
                (Let (Variable (Generated mon_53)) (Literal (Int 0))
                 (Operator (Variable (Generated mon_51)) (Int Greater_than)
                  (Variable (Generated mon_53)))))))))
           (Variable (Generated mon_45))
           (Lambda
            (((Variable (Generated mon_54)) (Variable (Generated mon_55)))
             (If_then_else (Variable (Generated mon_54))
              (Application (Variable (Language bind))
               ((Let (Variable (Generated mon_56)) (Variable (User h))
                 (Let (Variable (Generated mon_57)) (Literal (Int 0))
                  (Application (Variable (Generated mon_56))
                   ((Variable (Generated mon_57))
                    (Variable (Generated mon_55))))))
                (Variable (Generated mon_55))
                (Lambda
                 (((Variable (Generated mon_58)) (Variable (Generated mon_59)))
                  (Let (Variable (Generated mon_60)) (Literal (Int 1))
                   (Application (Variable (Generated mon_58))
                    ((Variable (Generated mon_60))
                     (Variable (Generated mon_59)))))))))
              (Application (Variable (Language bind))
               ((Let (Variable (Generated mon_61)) (Variable (User h))
                 (Let (Variable (Generated mon_62)) (Literal (Int 1))
                  (Application (Variable (Generated mon_61))
                   ((Variable (Generated mon_62))
                    (Variable (Generated mon_55))))))
                (Variable (Generated mon_55))
                (Lambda
                 (((Variable (Generated mon_63)) (Variable (Generated mon_64)))
                  (Let (Variable (Generated mon_65)) (Literal (Int 0))
                   (Application (Variable (Generated mon_63))
                    ((Variable (Generated mon_65))
                     (Variable (Generated mon_64))))))))))))))
         (Variable (Generated mon_45))
         (Lambda
          (((Variable (Generated mon_66)) (Variable (Generated mon_67)))
           (Let (Variable (User z)) (Variable (Generated mon_66))
            (Application (Variable (Language bind))
             ((Let (Variable (Generated mon_68)) (Variable (User h))
               (Let (Variable (Generated mon_69)) (Variable (User z))
                (Application (Variable (Generated mon_68))
                 ((Variable (Generated mon_69)) (Variable (Generated mon_67))))))
              (Variable (Generated mon_67))
              (Lambda
               (((Variable (Generated mon_70)) (Variable (Generated mon_71)))
                (Let (Variable (Generated mon_72)) (Variable (User z))
                 (Application (Variable (Generated mon_70))
                  ((Variable (Generated mon_72)) (Variable (Generated mon_71))))))))))))))))
     ((User main)
      (((Variable (Generated mon_73)))
       (Let (Variable (Generated mon_75))
        (Application (Variable (Language perform))
         ((Effect_label console)
          (Lambda
           (((Variable (Generated mon_74)))
            (Select_operation console (User println-int)
             (Variable (Generated mon_74)))))))
        (Application (Variable (Language bind))
         ((Let (Variable (Generated mon_76)) (Variable (User foo))
           (Let (Variable (Generated mon_80))
            (Lambda
             (((Variable (User y)) (Variable (Generated mon_77)))
              (Construct_pure
               (Let (Variable (Generated mon_78)) (Variable (User y))
                (Let (Variable (Generated mon_79)) (Variable (User y))
                 (Operator (Variable (Generated mon_78)) (Int Times)
                  (Variable (Generated mon_79))))))))
            (Let (Variable (Generated mon_84))
             (Lambda
              (((Variable (User x)) (Variable (Generated mon_81)))
               (Construct_pure
                (Let (Variable (Generated mon_82)) (Variable (User x))
                 (Let (Variable (Generated mon_83)) (Variable (User x))
                  (Operator (Variable (Generated mon_82)) (Int Plus)
                   (Variable (Generated mon_83))))))))
             (Let (Variable (Generated mon_89))
              (Lambda
               (((Variable (User m)) (Variable (Generated mon_85)))
                (Construct_pure
                 (Lambda
                  (((Variable (User n)) (Variable (Generated mon_86)))
                   (Construct_pure
                    (Let (Variable (Generated mon_87)) (Variable (User m))
                     (Let (Variable (Generated mon_88)) (Variable (User n))
                      (Operator (Variable (Generated mon_87)) (Int Minus)
                       (Variable (Generated mon_88)))))))))))
              (Let (Variable (Generated mon_90)) (Literal (Int 3))
               (Application (Variable (Generated mon_76))
                ((Variable (Generated mon_80)) (Variable (Generated mon_84))
                 (Variable (Generated mon_89)) (Variable (Generated mon_90))
                 (Variable (Generated mon_73)))))))))
          (Variable (Generated mon_73))
          (Lambda
           (((Variable (Generated mon_91)) (Variable (Generated mon_92)))
            (Application (Variable (Generated mon_75))
             ((Variable (Generated mon_91)) (Variable (Generated mon_92)))))))))))
     ((Language main)
      (((Variable (Generated mon_93)))
       (Application (Variable (Language bind))
        ((Let (Variable (Generated mon_100))
          (Application (Variable (Language handler))
           ((Effect_label console)
            (Construct_handler (handled_effect console)
             (operation_clauses
              (((User print-int)
                (Construct_op_tail
                 (Lambda
                  (((Variable (Language x)) (Variable (Generated mon_94)))
                   (Construct_pure
                    (Let (Variable (Generated mon_95)) (Variable (Language x))
                     (Impure_built_in
                      (Impure_print_int (value (Variable (Generated mon_95)))
                       (newline false)))))))))
               ((User println)
                (Construct_op_tail
                 (Lambda
                  ((Wildcard (Variable (Generated mon_96)))
                   (Construct_pure (Impure_built_in Impure_println))))))
               ((User println-int)
                (Construct_op_tail
                 (Lambda
                  (((Variable (Language x)) (Variable (Generated mon_97)))
                   (Construct_pure
                    (Let (Variable (Generated mon_98)) (Variable (Language x))
                     (Impure_built_in
                      (Impure_print_int (value (Variable (Generated mon_98)))
                       (newline true)))))))))
               ((User read-int)
                (Construct_op_tail
                 (Lambda
                  ((Wildcard (Variable (Generated mon_99)))
                   (Construct_pure (Impure_built_in Impure_read_int)))))))))))
          (Let (Variable (Generated mon_101)) (Variable (User main))
           (Application (Variable (Generated mon_100))
            ((Variable (Generated mon_101)) (Variable (Generated mon_93))))))
         (Variable (Generated mon_93))
         (Lambda
          (((Variable (Generated mon_102)) (Variable (Generated mon_103)))
           (Construct_pure (Literal Unit))))))))))
   (entry_expr (Application (Variable (Language main)) (Nil_evidence_vector))))

Then show with bind-inlining and other rewriting applied
  $ OPT_LEVEL=2 ../compile.sh many-binds.kk -dump-eps
  ((effect_declarations
    (((name console)
      (operations
       ((User print-int) (User println) (User println-int) (User read-int))))))
   (fun_declarations
    (((Language bind)
      (((Variable (Generated mon_0)) (Variable (Generated mon_1))
        (Variable (Generated mon_2)))
       (Match_ctl (subject (Variable (Generated mon_0)))
        (pure_branch
         ((Generated mon_3)
          (Application (Variable (Generated mon_2))
           ((Variable (Generated mon_3)) (Variable (Generated mon_1))))))
        (yield_branch
         ((Generated mon_4) (Generated mon_5) (Generated mon_6)
          (Construct_yield (marker (Variable (Generated mon_4)))
           (op_clause (Variable (Generated mon_5)))
           (resumption
            (Lambda
             (((Variable (Generated mon_7)) (Variable (Generated mon_8)))
              (Application (Variable (Language bind))
               ((Application (Variable (Generated mon_6))
                 ((Variable (Generated mon_7)) (Variable (Generated mon_8))))
                (Variable (Generated mon_8)) (Variable (Generated mon_2)))))))))))))
     ((Language prompt)
      (((Variable (Generated mon_9)) (Variable (Generated mon_10))
        (Variable (Generated mon_11)) (Variable (Generated mon_12))
        (Variable (Generated mon_13)))
       (Match_ctl
        (subject
         (Application (Variable (Generated mon_12))
          ((Cons_evidence_vector (label (Variable (Generated mon_9)))
            (marker (Variable (Generated mon_10)))
            (handler (Variable (Generated mon_11)))
            (handler_site_vector (Variable (Generated mon_13)))
            (vector_tail (Variable (Generated mon_13)))))))
        (pure_branch
         ((Generated mon_14) (Construct_pure (Variable (Generated mon_14)))))
        (yield_branch
         ((Generated mon_15) (Generated mon_16) (Generated mon_17)
          (If_then_else
           (Markers_equal (Variable (Generated mon_10))
            (Variable (Generated mon_15)))
           (Application (Variable (Generated mon_16))
            ((Lambda
              (((Variable (Generated mon_18)) (Variable (Generated mon_19)))
               (Application (Variable (Language prompt))
                ((Variable (Generated mon_9)) (Variable (Generated mon_10))
                 (Variable (Generated mon_11))
                 (Lambda
                  (((Variable (Generated mon_20)))
                   (Application (Variable (Generated mon_17))
                    ((Variable (Generated mon_18))
                     (Variable (Generated mon_20))))))
                 (Variable (Generated mon_19))))))
             (Variable (Generated mon_13))))
           (Construct_yield (marker (Variable (Generated mon_15)))
            (op_clause (Variable (Generated mon_16)))
            (resumption
             (Lambda
              (((Variable (Generated mon_18)) (Variable (Generated mon_19)))
               (Application (Variable (Language prompt))
                ((Variable (Generated mon_9)) (Variable (Generated mon_10))
                 (Variable (Generated mon_11))
                 (Lambda
                  (((Variable (Generated mon_20)))
                   (Application (Variable (Generated mon_17))
                    ((Variable (Generated mon_18))
                     (Variable (Generated mon_20))))))
                 (Variable (Generated mon_19))))))))))))))
     ((Language handler)
      (((Variable (Generated mon_21)) (Variable (Generated mon_22)))
       (Lambda
        (((Variable (Generated mon_23)) (Variable (Generated mon_24)))
         (Application (Variable (Language prompt))
          ((Variable (Generated mon_21)) Fresh_marker
           (Variable (Generated mon_22)) (Variable (Generated mon_23))
           (Variable (Generated mon_24))))))))
     ((Language under)
      (((Variable (Generated mon_25)) (Variable (Generated mon_26))
        (Variable (Generated mon_27)) (Variable (Generated mon_28)))
       (Match_ctl
        (subject
         (Application (Variable (Generated mon_28))
          ((Variable (Generated mon_27)) (Variable (Generated mon_26)))))
        (pure_branch
         ((Generated mon_29) (Construct_pure (Variable (Generated mon_29)))))
        (yield_branch
         ((Generated mon_30) (Generated mon_31) (Generated mon_32)
          (Construct_yield (marker (Variable (Generated mon_30)))
           (op_clause (Variable (Generated mon_31)))
           (resumption
            (Lambda
             (((Variable (Generated mon_33)) (Variable (Generated mon_34)))
              (Application (Variable (Language under))
               ((Variable (Generated mon_25))
                (Get_evidence_handler_site_vector
                 (Lookup_evidence (label (Variable (Generated mon_25)))
                  (vector (Variable (Generated mon_34)))))
                (Variable (Generated mon_33)) (Variable (Generated mon_32)))))))))))))
     ((Language perform)
      (((Variable (Generated mon_35)) (Variable (Generated mon_36)))
       (Lambda
        (((Variable (Generated mon_37)) (Variable (Generated mon_38)))
         (Match_op
          (subject
           (Application (Variable (Generated mon_36))
            ((Get_evidence_handler
              (Lookup_evidence (label (Variable (Generated mon_35)))
               (vector (Variable (Generated mon_38))))))))
          (normal_branch
           ((Generated mon_39)
            (Construct_yield
             (marker
              (Get_evidence_marker
               (Lookup_evidence (label (Variable (Generated mon_35)))
                (vector (Variable (Generated mon_38))))))
             (op_clause
              (Lambda
               (((Variable (Generated mon_42)) (Variable (Generated mon_43)))
                (Application (Variable (Generated mon_39))
                 ((Variable (Generated mon_37)) (Variable (Generated mon_42))
                  (Variable (Generated mon_43)))))))
             (resumption
              (Lambda
               (((Variable (Generated mon_40)) (Variable (Generated mon_41)))
                (Construct_pure (Variable (Generated mon_40)))))))))
          (tail_branch
           ((Generated mon_44)
            (Application (Variable (Language under))
             ((Variable (Generated mon_35))
              (Get_evidence_handler_site_vector
               (Lookup_evidence (label (Variable (Generated mon_35)))
                (vector (Variable (Generated mon_38)))))
              (Variable (Generated mon_37)) (Variable (Generated mon_44)))))))))))
     ((Generated opt_9)
      (((Variable (Generated mon_46)))
       (Lambda
        (((Variable (Generated mon_49)) (Variable (Generated mon_50)))
         (Application (Variable (Generated mon_46))
          ((Variable (Generated mon_49)) (Variable (Generated mon_50))))))))
     ((Generated opt_8)
      (()
       (Lambda
        (((Variable (Generated mon_51)) (Variable (Generated mon_52)))
         (Construct_pure
          (Let (Variable (Generated mon_53)) (Literal (Int 0))
           (Operator (Variable (Generated mon_51)) (Int Greater_than)
            (Variable (Generated mon_53)))))))))
     ((Generated opt_7)
      (((Variable (User h)))
       (Lambda
        (((Variable (Generated mon_54)) (Variable (Generated mon_55)))
         (If_then_else (Variable (Generated mon_54))
          (Application (Variable (Language bind))
           ((Let (Variable (Generated mon_56)) (Variable (User h))
             (Let (Variable (Generated mon_57)) (Literal (Int 0))
              (Application (Variable (Generated mon_56))
               ((Variable (Generated mon_57)) (Variable (Generated mon_55))))))
            (Variable (Generated mon_55))
            (Lambda
             (((Variable (Generated mon_58)) (Variable (Generated mon_59)))
              (Let (Variable (Generated mon_60)) (Literal (Int 1))
               (Application (Variable (Generated mon_58))
                ((Variable (Generated mon_60)) (Variable (Generated mon_59)))))))))
          (Application (Variable (Language bind))
           ((Let (Variable (Generated mon_61)) (Variable (User h))
             (Let (Variable (Generated mon_62)) (Literal (Int 1))
              (Application (Variable (Generated mon_61))
               ((Variable (Generated mon_62)) (Variable (Generated mon_55))))))
            (Variable (Generated mon_55))
            (Lambda
             (((Variable (Generated mon_63)) (Variable (Generated mon_64)))
              (Let (Variable (Generated mon_65)) (Literal (Int 0))
               (Application (Variable (Generated mon_63))
                ((Variable (Generated mon_65)) (Variable (Generated mon_64))))))))))))))
     ((Generated opt_0)
      (((Variable (User z)))
       (Lambda
        (((Variable (Generated mon_70)) (Variable (Generated mon_71)))
         (Let (Variable (Generated mon_72)) (Variable (User z))
          (Application (Variable (Generated mon_70))
           ((Variable (Generated mon_72)) (Variable (Generated mon_71)))))))))
     ((Generated opt_6)
      (((Variable (User h)))
       (Lambda
        (((Variable (Generated mon_66)) (Variable (Generated mon_67)))
         (Let (Variable (User z)) (Variable (Generated mon_66))
          (Application (Variable (Language bind))
           ((Let (Variable (Generated mon_68)) (Variable (User h))
             (Let (Variable (Generated mon_69)) (Variable (User z))
              (Application (Variable (Generated mon_68))
               ((Variable (Generated mon_69)) (Variable (Generated mon_67))))))
            (Variable (Generated mon_67))
            (Application (Variable (Generated opt_0)) ((Variable (User z)))))))))))
     ((User foo)
      (((Variable (User f)) (Variable (User g)) (Variable (User h))
        (Variable (User x)) (Variable (Generated mon_45)))
       (Match_ctl
        (subject
         (Match_ctl
          (subject
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_46)) (Variable (User f))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_47)) (Variable (User g))
                 (Let (Variable (Generated mon_48)) (Variable (User x))
                  (Application (Variable (Generated mon_47))
                   ((Variable (Generated mon_48))
                    (Variable (Generated mon_45)))))))
               (pure_branch
                ((Generated mon_49)
                 (Let (Variable (Generated mon_50))
                  (Variable (Generated mon_45))
                  (Application (Variable (Generated mon_46))
                   ((Variable (Generated mon_49))
                    (Variable (Generated mon_50)))))))
               (yield_branch
                ((Generated opt_10) (Generated opt_11) (Generated opt_12)
                 (Construct_yield (marker (Variable (Generated opt_10)))
                  (op_clause (Variable (Generated opt_11)))
                  (resumption
                   (Lambda
                    (((Variable (Generated opt_13))
                      (Variable (Generated opt_14)))
                     (Application (Variable (Language bind))
                      ((Application (Variable (Generated opt_12))
                        ((Variable (Generated opt_13))
                         (Variable (Generated opt_14))))
                       (Variable (Generated opt_14))
                       (Application (Variable (Generated opt_9))
                        ((Variable (Generated mon_46)))))))))))))))
            (pure_branch
             ((Generated mon_51)
              (Let (Variable (Generated mon_52)) (Variable (Generated mon_45))
               (Construct_pure
                (Let (Variable (Generated mon_53)) (Literal (Int 0))
                 (Operator (Variable (Generated mon_51)) (Int Greater_than)
                  (Variable (Generated mon_53))))))))
            (yield_branch
             ((Generated opt_15) (Generated opt_16) (Generated opt_17)
              (Construct_yield (marker (Variable (Generated opt_15)))
               (op_clause (Variable (Generated opt_16)))
               (resumption
                (Lambda
                 (((Variable (Generated opt_18)) (Variable (Generated opt_19)))
                  (Application (Variable (Language bind))
                   ((Application (Variable (Generated opt_17))
                     ((Variable (Generated opt_18))
                      (Variable (Generated opt_19))))
                    (Variable (Generated opt_19))
                    (Application (Variable (Generated opt_8)) ())))))))))))
          (pure_branch
           ((Generated mon_54)
            (Let (Variable (Generated mon_55)) (Variable (Generated mon_45))
             (If_then_else (Variable (Generated mon_54))
              (Application (Variable (Language bind))
               ((Let (Variable (Generated mon_56)) (Variable (User h))
                 (Let (Variable (Generated mon_57)) (Literal (Int 0))
                  (Application (Variable (Generated mon_56))
                   ((Variable (Generated mon_57))
                    (Variable (Generated mon_55))))))
                (Variable (Generated mon_55))
                (Lambda
                 (((Variable (Generated mon_58)) (Variable (Generated mon_59)))
                  (Let (Variable (Generated mon_60)) (Literal (Int 1))
                   (Application (Variable (Generated mon_58))
                    ((Variable (Generated mon_60))
                     (Variable (Generated mon_59)))))))))
              (Application (Variable (Language bind))
               ((Let (Variable (Generated mon_61)) (Variable (User h))
                 (Let (Variable (Generated mon_62)) (Literal (Int 1))
                  (Application (Variable (Generated mon_61))
                   ((Variable (Generated mon_62))
                    (Variable (Generated mon_55))))))
                (Variable (Generated mon_55))
                (Lambda
                 (((Variable (Generated mon_63)) (Variable (Generated mon_64)))
                  (Let (Variable (Generated mon_65)) (Literal (Int 0))
                   (Application (Variable (Generated mon_63))
                    ((Variable (Generated mon_65))
                     (Variable (Generated mon_64)))))))))))))
          (yield_branch
           ((Generated opt_20) (Generated opt_21) (Generated opt_22)
            (Construct_yield (marker (Variable (Generated opt_20)))
             (op_clause (Variable (Generated opt_21)))
             (resumption
              (Lambda
               (((Variable (Generated opt_23)) (Variable (Generated opt_24)))
                (Application (Variable (Language bind))
                 ((Application (Variable (Generated opt_22))
                   ((Variable (Generated opt_23))
                    (Variable (Generated opt_24))))
                  (Variable (Generated opt_24))
                  (Application (Variable (Generated opt_7))
                   ((Variable (User h))))))))))))))
        (pure_branch
         ((Generated mon_66)
          (Let (Variable (Generated mon_67)) (Variable (Generated mon_45))
           (Let (Variable (User z)) (Variable (Generated mon_66))
            (Match_ctl
             (subject
              (Let (Variable (Generated mon_68)) (Variable (User h))
               (Let (Variable (Generated mon_69)) (Variable (User z))
                (Application (Variable (Generated mon_68))
                 ((Variable (Generated mon_69)) (Variable (Generated mon_67)))))))
             (pure_branch
              ((Generated mon_70)
               (Let (Variable (Generated mon_71)) (Variable (Generated mon_67))
                (Let (Variable (Generated mon_72)) (Variable (User z))
                 (Application (Variable (Generated mon_70))
                  ((Variable (Generated mon_72)) (Variable (Generated mon_71))))))))
             (yield_branch
              ((Generated opt_1) (Generated opt_2) (Generated opt_3)
               (Construct_yield (marker (Variable (Generated opt_1)))
                (op_clause (Variable (Generated opt_2)))
                (resumption
                 (Lambda
                  (((Variable (Generated opt_4)) (Variable (Generated opt_5)))
                   (Application (Variable (Language bind))
                    ((Application (Variable (Generated opt_3))
                      ((Variable (Generated opt_4))
                       (Variable (Generated opt_5))))
                     (Variable (Generated opt_5))
                     (Application (Variable (Generated opt_0))
                      ((Variable (User z)))))))))))))))))
        (yield_branch
         ((Generated opt_25) (Generated opt_26) (Generated opt_27)
          (Construct_yield (marker (Variable (Generated opt_25)))
           (op_clause (Variable (Generated opt_26)))
           (resumption
            (Lambda
             (((Variable (Generated opt_28)) (Variable (Generated opt_29)))
              (Application (Variable (Language bind))
               ((Application (Variable (Generated opt_27))
                 ((Variable (Generated opt_28)) (Variable (Generated opt_29))))
                (Variable (Generated opt_29))
                (Application (Variable (Generated opt_6))
                 ((Variable (User h)))))))))))))))
     ((Generated opt_30)
      (((Variable (Generated mon_75)))
       (Lambda
        (((Variable (Generated mon_91)) (Variable (Generated mon_92)))
         (Application (Variable (Generated mon_75))
          ((Variable (Generated mon_91)) (Variable (Generated mon_92))))))))
     ((User main)
      (((Variable (Generated mon_73)))
       (Let (Variable (Generated mon_75))
        (Application (Variable (Language perform))
         ((Effect_label console)
          (Lambda
           (((Variable (Generated mon_74)))
            (Select_operation console (User println-int)
             (Variable (Generated mon_74)))))))
        (Match_ctl
         (subject
          (Let (Variable (Generated mon_76)) (Variable (User foo))
           (Let (Variable (Generated mon_80))
            (Lambda
             (((Variable (User y)) (Variable (Generated mon_77)))
              (Construct_pure
               (Let (Variable (Generated mon_78)) (Variable (User y))
                (Let (Variable (Generated mon_79)) (Variable (User y))
                 (Operator (Variable (Generated mon_78)) (Int Times)
                  (Variable (Generated mon_79))))))))
            (Let (Variable (Generated mon_84))
             (Lambda
              (((Variable (User x)) (Variable (Generated mon_81)))
               (Construct_pure
                (Let (Variable (Generated mon_82)) (Variable (User x))
                 (Let (Variable (Generated mon_83)) (Variable (User x))
                  (Operator (Variable (Generated mon_82)) (Int Plus)
                   (Variable (Generated mon_83))))))))
             (Let (Variable (Generated mon_89))
              (Lambda
               (((Variable (User m)) (Variable (Generated mon_85)))
                (Construct_pure
                 (Lambda
                  (((Variable (User n)) (Variable (Generated mon_86)))
                   (Construct_pure
                    (Let (Variable (Generated mon_87)) (Variable (User m))
                     (Let (Variable (Generated mon_88)) (Variable (User n))
                      (Operator (Variable (Generated mon_87)) (Int Minus)
                       (Variable (Generated mon_88)))))))))))
              (Let (Variable (Generated mon_90)) (Literal (Int 3))
               (Application (Variable (Generated mon_76))
                ((Variable (Generated mon_80)) (Variable (Generated mon_84))
                 (Variable (Generated mon_89)) (Variable (Generated mon_90))
                 (Variable (Generated mon_73))))))))))
         (pure_branch
          ((Generated mon_91)
           (Let (Variable (Generated mon_92)) (Variable (Generated mon_73))
            (Application (Variable (Generated mon_75))
             ((Variable (Generated mon_91)) (Variable (Generated mon_92)))))))
         (yield_branch
          ((Generated opt_31) (Generated opt_32) (Generated opt_33)
           (Construct_yield (marker (Variable (Generated opt_31)))
            (op_clause (Variable (Generated opt_32)))
            (resumption
             (Lambda
              (((Variable (Generated opt_34)) (Variable (Generated opt_35)))
               (Application (Variable (Language bind))
                ((Application (Variable (Generated opt_33))
                  ((Variable (Generated opt_34)) (Variable (Generated opt_35))))
                 (Variable (Generated opt_35))
                 (Application (Variable (Generated opt_30))
                  ((Variable (Generated mon_75))))))))))))))))
     ((Generated opt_36)
      (()
       (Lambda
        (((Variable (Generated mon_102)) (Variable (Generated mon_103)))
         (Construct_pure (Literal Unit))))))
     ((Language main)
      (((Variable (Generated mon_93)))
       (Match_ctl
        (subject
         (Let (Variable (Generated mon_100))
          (Application (Variable (Language handler))
           ((Effect_label console)
            (Construct_handler (handled_effect console)
             (operation_clauses
              (((User print-int)
                (Construct_op_tail
                 (Lambda
                  (((Variable (Language x)) (Variable (Generated mon_94)))
                   (Construct_pure
                    (Let (Variable (Generated mon_95)) (Variable (Language x))
                     (Impure_built_in
                      (Impure_print_int (value (Variable (Generated mon_95)))
                       (newline false)))))))))
               ((User println)
                (Construct_op_tail
                 (Lambda
                  ((Wildcard (Variable (Generated mon_96)))
                   (Construct_pure (Impure_built_in Impure_println))))))
               ((User println-int)
                (Construct_op_tail
                 (Lambda
                  (((Variable (Language x)) (Variable (Generated mon_97)))
                   (Construct_pure
                    (Let (Variable (Generated mon_98)) (Variable (Language x))
                     (Impure_built_in
                      (Impure_print_int (value (Variable (Generated mon_98)))
                       (newline true)))))))))
               ((User read-int)
                (Construct_op_tail
                 (Lambda
                  ((Wildcard (Variable (Generated mon_99)))
                   (Construct_pure (Impure_built_in Impure_read_int)))))))))))
          (Let (Variable (Generated mon_101)) (Variable (User main))
           (Application (Variable (Generated mon_100))
            ((Variable (Generated mon_101)) (Variable (Generated mon_93)))))))
        (pure_branch
         ((Generated mon_102)
          (Let (Variable (Generated mon_103)) (Variable (Generated mon_93))
           (Construct_pure (Literal Unit)))))
        (yield_branch
         ((Generated opt_37) (Generated opt_38) (Generated opt_39)
          (Construct_yield (marker (Variable (Generated opt_37)))
           (op_clause (Variable (Generated opt_38)))
           (resumption
            (Lambda
             (((Variable (Generated opt_40)) (Variable (Generated opt_41)))
              (Application (Variable (Language bind))
               ((Application (Variable (Generated opt_39))
                 ((Variable (Generated opt_40)) (Variable (Generated opt_41))))
                (Variable (Generated opt_41))
                (Application (Variable (Generated opt_36)) ()))))))))))))))
   (entry_expr (Application (Variable (Language main)) (Nil_evidence_vector))))
