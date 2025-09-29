Show the generated code for a program with many binds
  $ export PROJECT_ROOT=../../../..

First show without any rewriting optimisations
  $ ../../../koka-zero.sh compile-no-opt many-binds.kk -dump-eps
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
         ((Variable (Generated mon_3))
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
         ((Variable (Generated mon_14))
          (Construct_pure (Variable (Generated mon_14)))))
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
         ((Variable (Generated mon_29))
          (Construct_pure (Variable (Generated mon_29)))))
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
     ((User reverse)
      ((((Variable (User xs)) Pure) ((Variable (Generated mon_45)) Pure)) Ctl
       (Construct_pure
        (Let (Variable (User reverse-tail)) Pure
         (Fix_lambda
          ((User reverse-tail)
           ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
             ((Variable (Generated mon_46)) Pure))
            Ctl
            (Construct_pure
             (Let (Variable (Generated mon_47)) Pure (Variable (User xs))
              (Match (Variable (Generated mon_47)) List
               (((Construction List_nil ()) (Variable (User acc)))
                ((Construction List_cons
                  ((Variable (User x)) (Variable (User xx))))
                 (Let (Variable (Generated mon_48)) Pure
                  (Variable (User reverse-tail))
                  (Let (Variable (Generated mon_49)) Pure (Variable (User xx))
                   (Let (Variable (Generated mon_52)) Pure
                    (Let (Variable (Generated mon_50)) Pure (Variable (User x))
                     (Let (Variable (Generated mon_51)) Pure
                      (Variable (User acc))
                      (Construction List_cons
                       ((Variable (Generated mon_50))
                        (Variable (Generated mon_51))))))
                    (Match_ctl_pure
                     (subject
                      (Application (Variable (Generated mon_48))
                       (((Variable (Generated mon_49)) Pure)
                        ((Variable (Generated mon_52)) Pure)
                        ((Variable (Generated mon_46)) Pure))
                       Ctl))
                     (pure_branch
                      ((Variable (Generated mon_53))
                       (Variable (Generated mon_53))))))))))))))))
         (Let (Variable (Generated mon_54)) Pure (Variable (User reverse-tail))
          (Let (Variable (Generated mon_55)) Pure (Variable (User xs))
           (Let (Variable (Generated mon_56)) Pure (Construction List_nil ())
            (Match_ctl_pure
             (subject
              (Application (Variable (Generated mon_54))
               (((Variable (Generated mon_55)) Pure)
                ((Variable (Generated mon_56)) Pure)
                ((Variable (Generated mon_45)) Pure))
               Ctl))
             (pure_branch
              ((Variable (Generated mon_57)) (Variable (Generated mon_57))))))))))))
     ((User concat)
      ((((Variable (User xs)) Pure) ((Variable (User ys)) Pure)
        ((Variable (Generated mon_58)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (User concat-rev)) Pure
         (Fix_lambda
          ((User concat-rev)
           ((((Variable (User rev-xs)) Pure) ((Variable (User acc)) Pure)
             ((Variable (Generated mon_59)) Pure))
            Ctl
            (Construct_pure
             (Let (Variable (Generated mon_60)) Pure (Variable (User rev-xs))
              (Match (Variable (Generated mon_60)) List
               (((Construction List_nil ()) (Variable (User acc)))
                ((Construction List_cons
                  ((Variable (User x)) (Variable (User xx))))
                 (Let (Variable (Generated mon_61)) Pure
                  (Variable (User concat-rev))
                  (Let (Variable (Generated mon_62)) Pure (Variable (User xx))
                   (Let (Variable (Generated mon_65)) Pure
                    (Let (Variable (Generated mon_63)) Pure (Variable (User x))
                     (Let (Variable (Generated mon_64)) Pure
                      (Variable (User acc))
                      (Construction List_cons
                       ((Variable (Generated mon_63))
                        (Variable (Generated mon_64))))))
                    (Match_ctl_pure
                     (subject
                      (Application (Variable (Generated mon_61))
                       (((Variable (Generated mon_62)) Pure)
                        ((Variable (Generated mon_65)) Pure)
                        ((Variable (Generated mon_59)) Pure))
                       Ctl))
                     (pure_branch
                      ((Variable (Generated mon_66))
                       (Variable (Generated mon_66))))))))))))))))
         (Let (Variable (Generated mon_67)) Pure (Variable (User concat-rev))
          (Let (Variable (Generated mon_71)) Pure
           (Let (Variable (Generated mon_68)) Pure (Variable (User reverse))
            (Let (Variable (Generated mon_69)) Pure (Variable (User xs))
             (Match_ctl_pure
              (subject
               (Application (Variable (Generated mon_68))
                (((Variable (Generated mon_69)) Pure)
                 ((Variable (Generated mon_58)) Pure))
                Ctl))
              (pure_branch
               ((Variable (Generated mon_70)) (Variable (Generated mon_70)))))))
           (Let (Variable (Generated mon_72)) Pure (Variable (User ys))
            (Match_ctl_pure
             (subject
              (Application (Variable (Generated mon_67))
               (((Variable (Generated mon_71)) Pure)
                ((Variable (Generated mon_72)) Pure)
                ((Variable (Generated mon_58)) Pure))
               Ctl))
             (pure_branch
              ((Variable (Generated mon_73)) (Variable (Generated mon_73))))))))))))
     ((User flat-map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_74)) Pure))
       Ctl
       (Let (Variable (Generated mon_75)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_75)) List
         (((Construction List_nil ())
           (Construct_pure (Construction List_nil ())))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Let (Variable (Generated mon_76)) Pure (Variable (User concat))
            (Application (Variable (Language bind))
             (((Let (Variable (Generated mon_77)) Pure (Variable (User f))
                (Let (Variable (Generated mon_78)) Pure (Variable (User y))
                 (Application (Variable (Generated mon_77))
                  (((Variable (Generated mon_78)) Pure)
                   ((Variable (Generated mon_74)) Pure))
                  Ctl)))
               Ctl)
              ((Variable (Generated mon_74)) Pure)
              ((Lambda
                ((((Variable (Generated mon_79)) Pure)
                  ((Variable (Generated mon_80)) Pure))
                 Ctl
                 (Construct_pure
                  (Let (Variable (Generated mon_85)) Pure
                   (Let (Variable (Generated mon_81)) Pure
                    (Variable (User flat-map))
                    (Let (Variable (Generated mon_82)) Pure
                     (Variable (User ys))
                     (Let (Variable (Generated mon_83)) Pure
                      (Variable (User f))
                      (Match_ctl_pure
                       (subject
                        (Application (Variable (Generated mon_81))
                         (((Variable (Generated mon_82)) Pure)
                          ((Variable (Generated mon_83)) Pure)
                          ((Variable (Generated mon_80)) Pure))
                         Ctl))
                       (pure_branch
                        ((Variable (Generated mon_84))
                         (Variable (Generated mon_84))))))))
                   (Match_ctl_pure
                    (subject
                     (Application (Variable (Generated mon_76))
                      (((Variable (Generated mon_79)) Pure)
                       ((Variable (Generated mon_85)) Pure)
                       ((Variable (Generated mon_80)) Pure))
                      Ctl))
                    (pure_branch
                     ((Variable (Generated mon_86))
                      (Variable (Generated mon_86)))))))))
               Pure))
             Ctl))))))))
     ((User map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_87)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (User map-tail)) Pure
         (Fix_lambda
          ((User map-tail)
           ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
             ((Variable (User f)) Pure) ((Variable (Generated mon_88)) Pure))
            Ctl
            (Let (Variable (Generated mon_89)) Pure (Variable (User xs))
             (Match (Variable (Generated mon_89)) List
              (((Construction List_nil ())
                (Construct_pure
                 (Let (Variable (Generated mon_90)) Pure
                  (Variable (User reverse))
                  (Let (Variable (Generated mon_91)) Pure (Variable (User acc))
                   (Match_ctl_pure
                    (subject
                     (Application (Variable (Generated mon_90))
                      (((Variable (Generated mon_91)) Pure)
                       ((Variable (Generated mon_88)) Pure))
                      Ctl))
                    (pure_branch
                     ((Variable (Generated mon_92))
                      (Variable (Generated mon_92)))))))))
               ((Construction List_cons
                 ((Variable (User y)) (Variable (User ys))))
                (Let (Variable (Generated mon_93)) Pure
                 (Variable (User map-tail))
                 (Let (Variable (Generated mon_94)) Pure (Variable (User ys))
                  (Application (Variable (Language bind))
                   (((Application (Variable (Language bind))
                      (((Let (Variable (Generated mon_95)) Pure
                         (Variable (User f))
                         (Let (Variable (Generated mon_96)) Pure
                          (Variable (User y))
                          (Application (Variable (Generated mon_95))
                           (((Variable (Generated mon_96)) Pure)
                            ((Variable (Generated mon_88)) Pure))
                           Ctl)))
                        Ctl)
                       ((Variable (Generated mon_88)) Pure)
                       ((Lambda
                         ((((Variable (Generated mon_97)) Pure)
                           ((Variable (Generated mon_98)) Pure))
                          Ctl
                          (Construct_pure
                           (Let (Variable (Generated mon_99)) Pure
                            (Variable (User acc))
                            (Construction List_cons
                             ((Variable (Generated mon_97))
                              (Variable (Generated mon_99))))))))
                        Pure))
                      Ctl)
                     Ctl)
                    ((Variable (Generated mon_88)) Pure)
                    ((Lambda
                      ((((Variable (Generated mon_100)) Pure)
                        ((Variable (Generated mon_101)) Pure))
                       Ctl
                       (Construct_pure
                        (Let (Variable (Generated mon_102)) Pure
                         (Variable (User f))
                         (Match_ctl_pure
                          (subject
                           (Application (Variable (Generated mon_93))
                            (((Variable (Generated mon_94)) Pure)
                             ((Variable (Generated mon_100)) Pure)
                             ((Variable (Generated mon_102)) Pure)
                             ((Variable (Generated mon_101)) Pure))
                            Ctl))
                          (pure_branch
                           ((Variable (Generated mon_103))
                            (Variable (Generated mon_103)))))))))
                     Pure))
                   Ctl))))))))))
         (Let (Variable (Generated mon_104)) Pure (Variable (User map-tail))
          (Let (Variable (Generated mon_105)) Pure (Variable (User xs))
           (Let (Variable (Generated mon_106)) Pure (Construction List_nil ())
            (Let (Variable (Generated mon_107)) Pure (Variable (User f))
             (Match_ctl_pure
              (subject
               (Application (Variable (Generated mon_104))
                (((Variable (Generated mon_105)) Pure)
                 ((Variable (Generated mon_106)) Pure)
                 ((Variable (Generated mon_107)) Pure)
                 ((Variable (Generated mon_87)) Pure))
                Ctl))
              (pure_branch
               ((Variable (Generated mon_108)) (Variable (Generated mon_108)))))))))))))
     ((User exists)
      ((((Variable (User xs)) Pure) ((Variable (User p)) Pure)
        ((Variable (Generated mon_109)) Pure))
       Ctl
       (Let (Variable (Generated mon_110)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_110)) List
         (((Construction List_nil ()) (Construct_pure (Literal (Bool false))))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Application (Variable (Language bind))
            (((Let (Variable (Generated mon_111)) Pure (Variable (User p))
               (Let (Variable (Generated mon_112)) Pure (Variable (User y))
                (Application (Variable (Generated mon_111))
                 (((Variable (Generated mon_112)) Pure)
                  ((Variable (Generated mon_109)) Pure))
                 Ctl)))
              Ctl)
             ((Variable (Generated mon_109)) Pure)
             ((Lambda
               ((((Variable (Generated mon_113)) Pure)
                 ((Variable (Generated mon_114)) Pure))
                Ctl
                (Construct_pure
                 (If_then_else (Variable (Generated mon_113))
                  (Literal (Bool true))
                  (Let (Variable (Generated mon_115)) Pure
                   (Variable (User exists))
                   (Let (Variable (Generated mon_116)) Pure
                    (Variable (User ys))
                    (Let (Variable (Generated mon_117)) Pure
                     (Variable (User p))
                     (Match_ctl_pure
                      (subject
                       (Application (Variable (Generated mon_115))
                        (((Variable (Generated mon_116)) Pure)
                         ((Variable (Generated mon_117)) Pure)
                         ((Variable (Generated mon_114)) Pure))
                        Ctl))
                      (pure_branch
                       ((Variable (Generated mon_118))
                        (Variable (Generated mon_118))))))))))))
              Pure))
            Ctl)))))))
     ((User foo)
      ((((Variable (User f)) Pure) ((Variable (User g)) Pure)
        ((Variable (User h)) Pure) ((Variable (User x)) Pure)
        ((Variable (Generated mon_119)) Pure))
       Ctl
       (Application (Variable (Language bind))
        (((Application (Variable (Language bind))
           (((Application (Variable (Language bind))
              (((Let (Variable (Generated mon_120)) Pure (Variable (User f))
                 (Application (Variable (Language bind))
                  (((Let (Variable (Generated mon_121)) Pure
                     (Variable (User g))
                     (Let (Variable (Generated mon_122)) Pure
                      (Variable (User x))
                      (Application (Variable (Generated mon_121))
                       (((Variable (Generated mon_122)) Pure)
                        ((Variable (Generated mon_119)) Pure))
                       Ctl)))
                    Ctl)
                   ((Variable (Generated mon_119)) Pure)
                   ((Lambda
                     ((((Variable (Generated mon_123)) Pure)
                       ((Variable (Generated mon_124)) Pure))
                      Ctl
                      (Application (Variable (Generated mon_120))
                       (((Variable (Generated mon_123)) Pure)
                        ((Variable (Generated mon_124)) Pure))
                       Ctl)))
                    Pure))
                  Ctl))
                Ctl)
               ((Variable (Generated mon_119)) Pure)
               ((Lambda
                 ((((Variable (Generated mon_125)) Pure)
                   ((Variable (Generated mon_126)) Pure))
                  Ctl
                  (Construct_pure
                   (Let (Variable (Generated mon_127)) Pure (Literal (Int 0))
                    (Operator (Variable (Generated mon_125)) (Int Greater_than)
                     (Variable (Generated mon_127)))))))
                Pure))
              Ctl)
             Ctl)
            ((Variable (Generated mon_119)) Pure)
            ((Lambda
              ((((Variable (Generated mon_128)) Pure)
                ((Variable (Generated mon_129)) Pure))
               Ctl
               (If_then_else (Variable (Generated mon_128))
                (Application (Variable (Language bind))
                 (((Let (Variable (Generated mon_130)) Pure (Variable (User h))
                    (Let (Variable (Generated mon_131)) Pure (Literal (Int 0))
                     (Application (Variable (Generated mon_130))
                      (((Variable (Generated mon_131)) Pure)
                       ((Variable (Generated mon_129)) Pure))
                      Ctl)))
                   Ctl)
                  ((Variable (Generated mon_129)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_132)) Pure)
                      ((Variable (Generated mon_133)) Pure))
                     Ctl
                     (Let (Variable (Generated mon_134)) Pure (Literal (Int 1))
                      (Application (Variable (Generated mon_132))
                       (((Variable (Generated mon_134)) Pure)
                        ((Variable (Generated mon_133)) Pure))
                       Ctl))))
                   Pure))
                 Ctl)
                (Application (Variable (Language bind))
                 (((Let (Variable (Generated mon_135)) Pure (Variable (User h))
                    (Let (Variable (Generated mon_136)) Pure (Literal (Int 1))
                     (Application (Variable (Generated mon_135))
                      (((Variable (Generated mon_136)) Pure)
                       ((Variable (Generated mon_129)) Pure))
                      Ctl)))
                   Ctl)
                  ((Variable (Generated mon_129)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_137)) Pure)
                      ((Variable (Generated mon_138)) Pure))
                     Ctl
                     (Let (Variable (Generated mon_139)) Pure (Literal (Int 0))
                      (Application (Variable (Generated mon_137))
                       (((Variable (Generated mon_139)) Pure)
                        ((Variable (Generated mon_138)) Pure))
                       Ctl))))
                   Pure))
                 Ctl))))
             Pure))
           Ctl)
          Ctl)
         ((Variable (Generated mon_119)) Pure)
         ((Lambda
           ((((Variable (Generated mon_140)) Pure)
             ((Variable (Generated mon_141)) Pure))
            Ctl
            (Let (Variable (User z)) Pure (Variable (Generated mon_140))
             (Application (Variable (Language bind))
              (((Let (Variable (Generated mon_142)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_143)) Pure (Variable (User z))
                  (Application (Variable (Generated mon_142))
                   (((Variable (Generated mon_143)) Pure)
                    ((Variable (Generated mon_141)) Pure))
                   Ctl)))
                Ctl)
               ((Variable (Generated mon_141)) Pure)
               ((Lambda
                 ((((Variable (Generated mon_144)) Pure)
                   ((Variable (Generated mon_145)) Pure))
                  Ctl
                  (Let (Variable (Generated mon_146)) Pure (Variable (User z))
                   (Application (Variable (Generated mon_144))
                    (((Variable (Generated mon_146)) Pure)
                     ((Variable (Generated mon_145)) Pure))
                    Ctl))))
                Pure))
              Ctl))))
          Pure))
        Ctl)))
     ((User main)
      ((((Variable (Generated mon_147)) Pure)) Ctl
       (Let (Variable (Generated mon_149)) Pure
        (Application (Variable (Language perform))
         (((Effect_label console) Pure)
          ((Lambda
            ((((Variable (Generated mon_148)) Pure)) Pure
             (Select_operation console (User println-int)
              (Variable (Generated mon_148)))))
           Pure))
         Pure)
        (Let (Variable (Generated mon_166)) Pure
         (Let (Variable (Generated mon_150)) Pure (Variable (User foo))
          (Let (Variable (Generated mon_154)) Pure
           (Lambda
            ((((Variable (User y)) Pure) ((Variable (Generated mon_151)) Pure))
             Ctl
             (Construct_pure
              (Let (Variable (Generated mon_152)) Pure (Variable (User y))
               (Let (Variable (Generated mon_153)) Pure (Variable (User y))
                (Operator (Variable (Generated mon_152)) (Int Times)
                 (Variable (Generated mon_153))))))))
           (Let (Variable (Generated mon_158)) Pure
            (Lambda
             ((((Variable (User x)) Pure)
               ((Variable (Generated mon_155)) Pure))
              Ctl
              (Construct_pure
               (Let (Variable (Generated mon_156)) Pure (Variable (User x))
                (Let (Variable (Generated mon_157)) Pure (Variable (User x))
                 (Operator (Variable (Generated mon_156)) (Int Plus)
                  (Variable (Generated mon_157))))))))
            (Let (Variable (Generated mon_163)) Pure
             (Lambda
              ((((Variable (User m)) Pure)
                ((Variable (Generated mon_159)) Pure))
               Ctl
               (Construct_pure
                (Lambda
                 ((((Variable (User n)) Pure)
                   ((Variable (Generated mon_160)) Pure))
                  Ctl
                  (Construct_pure
                   (Let (Variable (Generated mon_161)) Pure (Variable (User m))
                    (Let (Variable (Generated mon_162)) Pure
                     (Variable (User n))
                     (Operator (Variable (Generated mon_161)) (Int Minus)
                      (Variable (Generated mon_162)))))))))))
             (Let (Variable (Generated mon_164)) Pure (Literal (Int 3))
              (Match_ctl_pure
               (subject
                (Application (Variable (Generated mon_150))
                 (((Variable (Generated mon_154)) Pure)
                  ((Variable (Generated mon_158)) Pure)
                  ((Variable (Generated mon_163)) Pure)
                  ((Variable (Generated mon_164)) Pure)
                  ((Variable (Generated mon_147)) Pure))
                 Ctl))
               (pure_branch
                ((Variable (Generated mon_165)) (Variable (Generated mon_165))))))))))
         (Application (Variable (Generated mon_149))
          (((Variable (Generated mon_166)) Pure)
           ((Variable (Generated mon_147)) Pure))
          Ctl)))))
     ((Language main)
      ((((Variable (Generated mon_167)) Pure)) Ctl
       (Application (Variable (Language bind))
        (((Let (Variable (Generated mon_174)) Pure
           (Application (Variable (Language handler))
            (((Effect_label console) Pure)
             ((Construct_handler (handled_effect console)
               (operation_clauses
                (((User print-int)
                  (Construct_op_tail
                   (Lambda
                    ((((Variable (Language x)) Pure)
                      ((Variable (Generated mon_168)) Pure))
                     Ctl
                     (Construct_pure
                      (Let (Variable (Generated mon_169)) Pure
                       (Variable (Language x))
                       (Impure_built_in
                        (Impure_print_int
                         (value (Variable (Generated mon_169)))
                         (newline false)))))))))
                 ((User println)
                  (Construct_op_tail
                   (Lambda
                    (((Wildcard Pure) ((Variable (Generated mon_170)) Pure))
                     Ctl (Construct_pure (Impure_built_in Impure_println))))))
                 ((User println-int)
                  (Construct_op_tail
                   (Lambda
                    ((((Variable (Language x)) Pure)
                      ((Variable (Generated mon_171)) Pure))
                     Ctl
                     (Construct_pure
                      (Let (Variable (Generated mon_172)) Pure
                       (Variable (Language x))
                       (Impure_built_in
                        (Impure_print_int
                         (value (Variable (Generated mon_172))) (newline true)))))))))
                 ((User read-int)
                  (Construct_op_tail
                   (Lambda
                    (((Wildcard Pure) ((Variable (Generated mon_173)) Pure))
                     Ctl (Construct_pure (Impure_built_in Impure_read_int)))))))))
              Pure))
            Pure)
           (Let (Variable (Generated mon_175)) Pure (Variable (User main))
            (Application (Variable (Generated mon_174))
             (((Variable (Generated mon_175)) Pure)
              ((Variable (Generated mon_167)) Pure))
             Ctl)))
          Ctl)
         ((Variable (Generated mon_167)) Pure)
         ((Lambda
           ((((Variable (Generated mon_176)) Pure)
             ((Variable (Generated mon_177)) Pure))
            Ctl (Construct_pure (Tuple_construction ()))))
          Pure))
        Ctl)))))
   (entry_expr
    (Application (Variable (Language main)) ((Nil_evidence_vector Pure)) Ctl)))

Then show with bind-inlining and other rewriting applied
  $ ../../../koka-zero.sh compile many-binds.kk -dump-eps
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
         ((Variable (Generated mon_3))
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
         ((Variable (Generated mon_14))
          (Construct_pure (Variable (Generated mon_14)))))
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
         ((Variable (Generated mon_29))
          (Construct_pure (Variable (Generated mon_29)))))
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
     ((User reverse)
      ((((Variable (User xs)) Pure) ((Variable (Generated mon_45)) Pure)) Ctl
       (Let (Variable (User reverse-tail)) Pure
        (Fix_lambda
         ((User reverse-tail)
          ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
            ((Variable (Generated mon_46)) Pure))
           Ctl
           (Let (Variable (Generated mon_47)) Pure (Variable (User xs))
            (Match (Variable (Generated mon_47)) List
             (((Construction List_nil ())
               (Construct_pure (Variable (User acc))))
              ((Construction List_cons
                ((Variable (User x)) (Variable (User xx))))
               (Let (Variable (Generated mon_48)) Pure
                (Variable (User reverse-tail))
                (Let (Variable (Generated mon_49)) Pure (Variable (User xx))
                 (Let (Variable (Generated mon_52)) Pure
                  (Let (Variable (Generated mon_50)) Pure (Variable (User x))
                   (Let (Variable (Generated mon_51)) Pure
                    (Variable (User acc))
                    (Construction List_cons
                     ((Variable (Generated mon_50))
                      (Variable (Generated mon_51))))))
                  (Application (Variable (Generated mon_48))
                   (((Variable (Generated mon_49)) Pure)
                    ((Variable (Generated mon_52)) Pure)
                    ((Variable (Generated mon_46)) Pure))
                   Ctl)))))))))))
        (Let (Variable (Generated mon_54)) Pure (Variable (User reverse-tail))
         (Let (Variable (Generated mon_55)) Pure (Variable (User xs))
          (Let (Variable (Generated mon_56)) Pure (Construction List_nil ())
           (Application (Variable (Generated mon_54))
            (((Variable (Generated mon_55)) Pure)
             ((Variable (Generated mon_56)) Pure)
             ((Variable (Generated mon_45)) Pure))
            Ctl)))))))
     ((User concat)
      ((((Variable (User xs)) Pure) ((Variable (User ys)) Pure)
        ((Variable (Generated mon_58)) Pure))
       Ctl
       (Let (Variable (User concat-rev)) Pure
        (Fix_lambda
         ((User concat-rev)
          ((((Variable (User rev-xs)) Pure) ((Variable (User acc)) Pure)
            ((Variable (Generated mon_59)) Pure))
           Ctl
           (Let (Variable (Generated mon_60)) Pure (Variable (User rev-xs))
            (Match (Variable (Generated mon_60)) List
             (((Construction List_nil ())
               (Construct_pure (Variable (User acc))))
              ((Construction List_cons
                ((Variable (User x)) (Variable (User xx))))
               (Let (Variable (Generated mon_61)) Pure
                (Variable (User concat-rev))
                (Let (Variable (Generated mon_62)) Pure (Variable (User xx))
                 (Let (Variable (Generated mon_65)) Pure
                  (Let (Variable (Generated mon_63)) Pure (Variable (User x))
                   (Let (Variable (Generated mon_64)) Pure
                    (Variable (User acc))
                    (Construction List_cons
                     ((Variable (Generated mon_63))
                      (Variable (Generated mon_64))))))
                  (Application (Variable (Generated mon_61))
                   (((Variable (Generated mon_62)) Pure)
                    ((Variable (Generated mon_65)) Pure)
                    ((Variable (Generated mon_59)) Pure))
                   Ctl)))))))))))
        (Let (Variable (Generated mon_67)) Pure (Variable (User concat-rev))
         (Let (Variable (Generated mon_71)) Pure
          (Let (Variable (Generated mon_68)) Pure (Variable (User reverse))
           (Let (Variable (Generated mon_69)) Pure (Variable (User xs))
            (Match_ctl_pure
             (subject
              (Application (Variable (Generated mon_68))
               (((Variable (Generated mon_69)) Pure)
                ((Variable (Generated mon_58)) Pure))
               Ctl))
             (pure_branch
              ((Variable (Generated mon_70)) (Variable (Generated mon_70)))))))
          (Let (Variable (Generated mon_72)) Pure (Variable (User ys))
           (Application (Variable (Generated mon_67))
            (((Variable (Generated mon_71)) Pure)
             ((Variable (Generated mon_72)) Pure)
             ((Variable (Generated mon_58)) Pure))
            Ctl)))))))
     ((Generated opt_0)
      ((((Variable (User f)) Pure) ((Variable (User flat-map)) Pure)
        ((Variable (User ys)) Pure) ((Variable (Generated mon_76)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_79)) Pure)
          ((Variable (Generated mon_80)) Pure))
         Ctl
         (Let (Variable (Generated mon_85)) Pure
          (Let (Variable (Generated mon_81)) Pure (Variable (User flat-map))
           (Let (Variable (Generated mon_82)) Pure (Variable (User ys))
            (Let (Variable (Generated mon_83)) Pure (Variable (User f))
             (Match_ctl_pure
              (subject
               (Application (Variable (Generated mon_81))
                (((Variable (Generated mon_82)) Pure)
                 ((Variable (Generated mon_83)) Pure)
                 ((Variable (Generated mon_80)) Pure))
                Ctl))
              (pure_branch
               ((Variable (Generated mon_84)) (Variable (Generated mon_84))))))))
          (Application (Variable (Generated mon_76))
           (((Variable (Generated mon_79)) Pure)
            ((Variable (Generated mon_85)) Pure)
            ((Variable (Generated mon_80)) Pure))
           Ctl))))))
     ((User flat-map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_74)) Pure))
       Ctl
       (Let (Variable (Generated mon_75)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_75)) List
         (((Construction List_nil ())
           (Construct_pure (Construction List_nil ())))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Let (Variable (Generated mon_76)) Pure (Variable (User concat))
            (Match_ctl
             (subject
              (Let (Variable (Generated mon_77)) Pure (Variable (User f))
               (Let (Variable (Generated mon_78)) Pure (Variable (User y))
                (Application (Variable (Generated mon_77))
                 (((Variable (Generated mon_78)) Pure)
                  ((Variable (Generated mon_74)) Pure))
                 Ctl))))
             (pure_branch
              ((Variable (Generated mon_79))
               (Let (Variable (Generated mon_80)) Pure
                (Variable (Generated mon_74))
                (Let (Variable (Generated mon_85)) Pure
                 (Let (Variable (Generated mon_81)) Pure
                  (Variable (User flat-map))
                  (Let (Variable (Generated mon_82)) Pure (Variable (User ys))
                   (Let (Variable (Generated mon_83)) Pure (Variable (User f))
                    (Match_ctl_pure
                     (subject
                      (Application (Variable (Generated mon_81))
                       (((Variable (Generated mon_82)) Pure)
                        ((Variable (Generated mon_83)) Pure)
                        ((Variable (Generated mon_80)) Pure))
                       Ctl))
                     (pure_branch
                      ((Variable (Generated mon_84))
                       (Variable (Generated mon_84))))))))
                 (Application (Variable (Generated mon_76))
                  (((Variable (Generated mon_79)) Pure)
                   ((Variable (Generated mon_85)) Pure)
                   ((Variable (Generated mon_80)) Pure))
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
                       (((Variable (User f)) Pure)
                        ((Variable (User flat-map)) Pure)
                        ((Variable (User ys)) Pure)
                        ((Variable (Generated mon_76)) Pure))
                       Pure)
                      Pure))
                    Ctl)))))))))))))))
     ((Generated opt_7)
      ((((Variable (User acc)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_97)) Pure)
          ((Variable (Generated mon_98)) Pure))
         Ctl
         (Let (Variable (Generated mon_99)) Pure (Variable (User acc))
          (Construct_pure
           (Construction List_cons
            ((Variable (Generated mon_97)) (Variable (Generated mon_99))))))))))
     ((Generated opt_6)
      ((((Variable (User f)) Pure) ((Variable (Generated mon_93)) Pure)
        ((Variable (Generated mon_94)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_100)) Pure)
          ((Variable (Generated mon_101)) Pure))
         Ctl
         (Let (Variable (Generated mon_102)) Pure (Variable (User f))
          (Application (Variable (Generated mon_93))
           (((Variable (Generated mon_94)) Pure)
            ((Variable (Generated mon_100)) Pure)
            ((Variable (Generated mon_102)) Pure)
            ((Variable (Generated mon_101)) Pure))
           Ctl))))))
     ((User map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_87)) Pure))
       Ctl
       (Let (Variable (User map-tail)) Pure
        (Fix_lambda
         ((User map-tail)
          ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
            ((Variable (User f)) Pure) ((Variable (Generated mon_88)) Pure))
           Ctl
           (Let (Variable (Generated mon_89)) Pure (Variable (User xs))
            (Match (Variable (Generated mon_89)) List
             (((Construction List_nil ())
               (Let (Variable (Generated mon_90)) Pure
                (Variable (User reverse))
                (Let (Variable (Generated mon_91)) Pure (Variable (User acc))
                 (Application (Variable (Generated mon_90))
                  (((Variable (Generated mon_91)) Pure)
                   ((Variable (Generated mon_88)) Pure))
                  Ctl))))
              ((Construction List_cons
                ((Variable (User y)) (Variable (User ys))))
               (Let (Variable (Generated mon_93)) Pure
                (Variable (User map-tail))
                (Let (Variable (Generated mon_94)) Pure (Variable (User ys))
                 (Match_ctl
                  (subject
                   (Match_ctl
                    (subject
                     (Let (Variable (Generated mon_95)) Pure
                      (Variable (User f))
                      (Let (Variable (Generated mon_96)) Pure
                       (Variable (User y))
                       (Application (Variable (Generated mon_95))
                        (((Variable (Generated mon_96)) Pure)
                         ((Variable (Generated mon_88)) Pure))
                        Ctl))))
                    (pure_branch
                     ((Variable (Generated mon_97))
                      (Let (Variable (Generated mon_98)) Pure
                       (Variable (Generated mon_88))
                       (Let (Variable (Generated mon_99)) Pure
                        (Variable (User acc))
                        (Construct_pure
                         (Construction List_cons
                          ((Variable (Generated mon_97))
                           (Variable (Generated mon_99)))))))))
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
                            ((Application (Variable (Generated opt_7))
                              (((Variable (User acc)) Pure)) Pure)
                             Pure))
                           Ctl)))))))))
                  (pure_branch
                   ((Variable (Generated mon_100))
                    (Let (Variable (Generated mon_101)) Pure
                     (Variable (Generated mon_88))
                     (Let (Variable (Generated mon_102)) Pure
                      (Variable (User f))
                      (Application (Variable (Generated mon_93))
                       (((Variable (Generated mon_94)) Pure)
                        ((Variable (Generated mon_100)) Pure)
                        ((Variable (Generated mon_102)) Pure)
                        ((Variable (Generated mon_101)) Pure))
                       Ctl)))))
                  (yield_branch
                   ((Generated opt_13) (Generated opt_14) (Generated opt_15)
                    (Construct_yield (marker (Variable (Generated opt_13)))
                     (op_clause (Variable (Generated opt_14)))
                     (resumption
                      (Lambda
                       ((((Variable (Generated opt_16)) Pure)
                         ((Variable (Generated opt_17)) Pure))
                        Ctl
                        (Application (Variable (Language bind))
                         (((Application (Variable (Generated opt_15))
                            (((Variable (Generated opt_16)) Pure)
                             ((Variable (Generated opt_17)) Pure))
                            Ctl)
                           Ctl)
                          ((Variable (Generated opt_17)) Pure)
                          ((Application (Variable (Generated opt_6))
                            (((Variable (User f)) Pure)
                             ((Variable (Generated mon_93)) Pure)
                             ((Variable (Generated mon_94)) Pure))
                            Pure)
                           Pure))
                         Ctl)))))))))))))))))
        (Let (Variable (Generated mon_104)) Pure (Variable (User map-tail))
         (Let (Variable (Generated mon_105)) Pure (Variable (User xs))
          (Let (Variable (Generated mon_106)) Pure (Construction List_nil ())
           (Let (Variable (Generated mon_107)) Pure (Variable (User f))
            (Application (Variable (Generated mon_104))
             (((Variable (Generated mon_105)) Pure)
              ((Variable (Generated mon_106)) Pure)
              ((Variable (Generated mon_107)) Pure)
              ((Variable (Generated mon_87)) Pure))
             Ctl))))))))
     ((Generated opt_18)
      ((((Variable (User exists)) Pure) ((Variable (User p)) Pure)
        ((Variable (User ys)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_113)) Pure)
          ((Variable (Generated mon_114)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_113))
          (Construct_pure (Literal (Bool true)))
          (Let (Variable (Generated mon_115)) Pure (Variable (User exists))
           (Let (Variable (Generated mon_116)) Pure (Variable (User ys))
            (Let (Variable (Generated mon_117)) Pure (Variable (User p))
             (Application (Variable (Generated mon_115))
              (((Variable (Generated mon_116)) Pure)
               ((Variable (Generated mon_117)) Pure)
               ((Variable (Generated mon_114)) Pure))
              Ctl)))))))))
     ((User exists)
      ((((Variable (User xs)) Pure) ((Variable (User p)) Pure)
        ((Variable (Generated mon_109)) Pure))
       Ctl
       (Let (Variable (Generated mon_110)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_110)) List
         (((Construction List_nil ()) (Construct_pure (Literal (Bool false))))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_111)) Pure (Variable (User p))
              (Let (Variable (Generated mon_112)) Pure (Variable (User y))
               (Application (Variable (Generated mon_111))
                (((Variable (Generated mon_112)) Pure)
                 ((Variable (Generated mon_109)) Pure))
                Ctl))))
            (pure_branch
             ((Variable (Generated mon_113))
              (Let (Variable (Generated mon_114)) Pure
               (Variable (Generated mon_109))
               (If_then_else (Variable (Generated mon_113))
                (Construct_pure (Literal (Bool true)))
                (Let (Variable (Generated mon_115)) Pure
                 (Variable (User exists))
                 (Let (Variable (Generated mon_116)) Pure (Variable (User ys))
                  (Let (Variable (Generated mon_117)) Pure (Variable (User p))
                   (Application (Variable (Generated mon_115))
                    (((Variable (Generated mon_116)) Pure)
                     ((Variable (Generated mon_117)) Pure)
                     ((Variable (Generated mon_114)) Pure))
                    Ctl))))))))
            (yield_branch
             ((Generated opt_19) (Generated opt_20) (Generated opt_21)
              (Construct_yield (marker (Variable (Generated opt_19)))
               (op_clause (Variable (Generated opt_20)))
               (resumption
                (Lambda
                 ((((Variable (Generated opt_22)) Pure)
                   ((Variable (Generated opt_23)) Pure))
                  Ctl
                  (Application (Variable (Language bind))
                   (((Application (Variable (Generated opt_21))
                      (((Variable (Generated opt_22)) Pure)
                       ((Variable (Generated opt_23)) Pure))
                      Ctl)
                     Ctl)
                    ((Variable (Generated opt_23)) Pure)
                    ((Application (Variable (Generated opt_18))
                      (((Variable (User exists)) Pure)
                       ((Variable (User p)) Pure) ((Variable (User ys)) Pure))
                      Pure)
                     Pure))
                   Ctl))))))))))))))
     ((Generated opt_45)
      ((((Variable (Generated mon_120)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_123)) Pure)
          ((Variable (Generated mon_124)) Pure))
         Ctl
         (Application (Variable (Generated mon_120))
          (((Variable (Generated mon_123)) Pure)
           ((Variable (Generated mon_124)) Pure))
          Ctl)))))
     ((Generated opt_44)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_125)) Pure)
          ((Variable (Generated mon_126)) Pure))
         Ctl
         (Let (Variable (Generated mon_127)) Pure (Literal (Int 0))
          (Construct_pure
           (Operator (Variable (Generated mon_125)) (Int Greater_than)
            (Variable (Generated mon_127)))))))))
     ((Generated opt_31)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_132)) Pure)
          ((Variable (Generated mon_133)) Pure))
         Ctl
         (Let (Variable (Generated mon_134)) Pure (Literal (Int 1))
          (Application (Variable (Generated mon_132))
           (((Variable (Generated mon_134)) Pure)
            ((Variable (Generated mon_133)) Pure))
           Ctl))))))
     ((Generated opt_37)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_137)) Pure)
          ((Variable (Generated mon_138)) Pure))
         Ctl
         (Let (Variable (Generated mon_139)) Pure (Literal (Int 0))
          (Application (Variable (Generated mon_137))
           (((Variable (Generated mon_139)) Pure)
            ((Variable (Generated mon_138)) Pure))
           Ctl))))))
     ((Generated opt_43)
      ((((Variable (User h)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_128)) Pure)
          ((Variable (Generated mon_129)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_128))
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_130)) Pure (Variable (User h))
              (Let (Variable (Generated mon_131)) Pure (Literal (Int 0))
               (Application (Variable (Generated mon_130))
                (((Variable (Generated mon_131)) Pure)
                 ((Variable (Generated mon_129)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_129)) Pure)
            ((Application (Variable (Generated opt_31)) () Pure) Pure))
           Ctl)
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_135)) Pure (Variable (User h))
              (Let (Variable (Generated mon_136)) Pure (Literal (Int 1))
               (Application (Variable (Generated mon_135))
                (((Variable (Generated mon_136)) Pure)
                 ((Variable (Generated mon_129)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_129)) Pure)
            ((Application (Variable (Generated opt_37)) () Pure) Pure))
           Ctl))))))
     ((Generated opt_24)
      ((((Variable (User z)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_144)) Pure)
          ((Variable (Generated mon_145)) Pure))
         Ctl
         (Let (Variable (Generated mon_146)) Pure (Variable (User z))
          (Application (Variable (Generated mon_144))
           (((Variable (Generated mon_146)) Pure)
            ((Variable (Generated mon_145)) Pure))
           Ctl))))))
     ((Generated opt_30)
      ((((Variable (User h)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_140)) Pure)
          ((Variable (Generated mon_141)) Pure))
         Ctl
         (Let (Variable (User z)) Pure (Variable (Generated mon_140))
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_142)) Pure (Variable (User h))
              (Let (Variable (Generated mon_143)) Pure (Variable (User z))
               (Application (Variable (Generated mon_142))
                (((Variable (Generated mon_143)) Pure)
                 ((Variable (Generated mon_141)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_141)) Pure)
            ((Application (Variable (Generated opt_24))
              (((Variable (User z)) Pure)) Pure)
             Pure))
           Ctl))))))
     ((User foo)
      ((((Variable (User f)) Pure) ((Variable (User g)) Pure)
        ((Variable (User h)) Pure) ((Variable (User x)) Pure)
        ((Variable (Generated mon_119)) Pure))
       Ctl
       (Match_ctl
        (subject
         (Match_ctl
          (subject
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_120)) Pure (Variable (User f))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_121)) Pure (Variable (User g))
                 (Let (Variable (Generated mon_122)) Pure (Variable (User x))
                  (Application (Variable (Generated mon_121))
                   (((Variable (Generated mon_122)) Pure)
                    ((Variable (Generated mon_119)) Pure))
                   Ctl))))
               (pure_branch
                ((Variable (Generated mon_123))
                 (Let (Variable (Generated mon_124)) Pure
                  (Variable (Generated mon_119))
                  (Application (Variable (Generated mon_120))
                   (((Variable (Generated mon_123)) Pure)
                    ((Variable (Generated mon_124)) Pure))
                   Ctl))))
               (yield_branch
                ((Generated opt_46) (Generated opt_47) (Generated opt_48)
                 (Construct_yield (marker (Variable (Generated opt_46)))
                  (op_clause (Variable (Generated opt_47)))
                  (resumption
                   (Lambda
                    ((((Variable (Generated opt_49)) Pure)
                      ((Variable (Generated opt_50)) Pure))
                     Ctl
                     (Application (Variable (Language bind))
                      (((Application (Variable (Generated opt_48))
                         (((Variable (Generated opt_49)) Pure)
                          ((Variable (Generated opt_50)) Pure))
                         Ctl)
                        Ctl)
                       ((Variable (Generated opt_50)) Pure)
                       ((Application (Variable (Generated opt_45))
                         (((Variable (Generated mon_120)) Pure)) Pure)
                        Pure))
                      Ctl))))))))))
            (pure_branch
             ((Variable (Generated mon_125))
              (Let (Variable (Generated mon_126)) Pure
               (Variable (Generated mon_119))
               (Let (Variable (Generated mon_127)) Pure (Literal (Int 0))
                (Construct_pure
                 (Operator (Variable (Generated mon_125)) (Int Greater_than)
                  (Variable (Generated mon_127))))))))
            (yield_branch
             ((Generated opt_51) (Generated opt_52) (Generated opt_53)
              (Construct_yield (marker (Variable (Generated opt_51)))
               (op_clause (Variable (Generated opt_52)))
               (resumption
                (Lambda
                 ((((Variable (Generated opt_54)) Pure)
                   ((Variable (Generated opt_55)) Pure))
                  Ctl
                  (Application (Variable (Language bind))
                   (((Application (Variable (Generated opt_53))
                      (((Variable (Generated opt_54)) Pure)
                       ((Variable (Generated opt_55)) Pure))
                      Ctl)
                     Ctl)
                    ((Variable (Generated opt_55)) Pure)
                    ((Application (Variable (Generated opt_44)) () Pure) Pure))
                   Ctl)))))))))
          (pure_branch
           ((Variable (Generated mon_128))
            (Let (Variable (Generated mon_129)) Pure
             (Variable (Generated mon_119))
             (If_then_else (Variable (Generated mon_128))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_130)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_131)) Pure (Literal (Int 0))
                  (Application (Variable (Generated mon_130))
                   (((Variable (Generated mon_131)) Pure)
                    ((Variable (Generated mon_129)) Pure))
                   Ctl))))
               (pure_branch
                ((Variable (Generated mon_132))
                 (Let (Variable (Generated mon_133)) Pure
                  (Variable (Generated mon_129))
                  (Let (Variable (Generated mon_134)) Pure (Literal (Int 1))
                   (Application (Variable (Generated mon_132))
                    (((Variable (Generated mon_134)) Pure)
                     ((Variable (Generated mon_133)) Pure))
                    Ctl)))))
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
                       ((Application (Variable (Generated opt_31)) () Pure)
                        Pure))
                      Ctl))))))))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_135)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_136)) Pure (Literal (Int 1))
                  (Application (Variable (Generated mon_135))
                   (((Variable (Generated mon_136)) Pure)
                    ((Variable (Generated mon_129)) Pure))
                   Ctl))))
               (pure_branch
                ((Variable (Generated mon_137))
                 (Let (Variable (Generated mon_138)) Pure
                  (Variable (Generated mon_129))
                  (Let (Variable (Generated mon_139)) Pure (Literal (Int 0))
                   (Application (Variable (Generated mon_137))
                    (((Variable (Generated mon_139)) Pure)
                     ((Variable (Generated mon_138)) Pure))
                    Ctl)))))
               (yield_branch
                ((Generated opt_38) (Generated opt_39) (Generated opt_40)
                 (Construct_yield (marker (Variable (Generated opt_38)))
                  (op_clause (Variable (Generated opt_39)))
                  (resumption
                   (Lambda
                    ((((Variable (Generated opt_41)) Pure)
                      ((Variable (Generated opt_42)) Pure))
                     Ctl
                     (Application (Variable (Language bind))
                      (((Application (Variable (Generated opt_40))
                         (((Variable (Generated opt_41)) Pure)
                          ((Variable (Generated opt_42)) Pure))
                         Ctl)
                        Ctl)
                       ((Variable (Generated opt_42)) Pure)
                       ((Application (Variable (Generated opt_37)) () Pure)
                        Pure))
                      Ctl))))))))))))
          (yield_branch
           ((Generated opt_56) (Generated opt_57) (Generated opt_58)
            (Construct_yield (marker (Variable (Generated opt_56)))
             (op_clause (Variable (Generated opt_57)))
             (resumption
              (Lambda
               ((((Variable (Generated opt_59)) Pure)
                 ((Variable (Generated opt_60)) Pure))
                Ctl
                (Application (Variable (Language bind))
                 (((Application (Variable (Generated opt_58))
                    (((Variable (Generated opt_59)) Pure)
                     ((Variable (Generated opt_60)) Pure))
                    Ctl)
                   Ctl)
                  ((Variable (Generated opt_60)) Pure)
                  ((Application (Variable (Generated opt_43))
                    (((Variable (User h)) Pure)) Pure)
                   Pure))
                 Ctl)))))))))
        (pure_branch
         ((Variable (Generated mon_140))
          (Let (Variable (Generated mon_141)) Pure
           (Variable (Generated mon_119))
           (Let (Variable (User z)) Pure (Variable (Generated mon_140))
            (Match_ctl
             (subject
              (Let (Variable (Generated mon_142)) Pure (Variable (User h))
               (Let (Variable (Generated mon_143)) Pure (Variable (User z))
                (Application (Variable (Generated mon_142))
                 (((Variable (Generated mon_143)) Pure)
                  ((Variable (Generated mon_141)) Pure))
                 Ctl))))
             (pure_branch
              ((Variable (Generated mon_144))
               (Let (Variable (Generated mon_145)) Pure
                (Variable (Generated mon_141))
                (Let (Variable (Generated mon_146)) Pure (Variable (User z))
                 (Application (Variable (Generated mon_144))
                  (((Variable (Generated mon_146)) Pure)
                   ((Variable (Generated mon_145)) Pure))
                  Ctl)))))
             (yield_branch
              ((Generated opt_25) (Generated opt_26) (Generated opt_27)
               (Construct_yield (marker (Variable (Generated opt_25)))
                (op_clause (Variable (Generated opt_26)))
                (resumption
                 (Lambda
                  ((((Variable (Generated opt_28)) Pure)
                    ((Variable (Generated opt_29)) Pure))
                   Ctl
                   (Application (Variable (Language bind))
                    (((Application (Variable (Generated opt_27))
                       (((Variable (Generated opt_28)) Pure)
                        ((Variable (Generated opt_29)) Pure))
                       Ctl)
                      Ctl)
                     ((Variable (Generated opt_29)) Pure)
                     ((Application (Variable (Generated opt_24))
                       (((Variable (User z)) Pure)) Pure)
                      Pure))
                    Ctl))))))))))))
        (yield_branch
         ((Generated opt_61) (Generated opt_62) (Generated opt_63)
          (Construct_yield (marker (Variable (Generated opt_61)))
           (op_clause (Variable (Generated opt_62)))
           (resumption
            (Lambda
             ((((Variable (Generated opt_64)) Pure)
               ((Variable (Generated opt_65)) Pure))
              Ctl
              (Application (Variable (Language bind))
               (((Application (Variable (Generated opt_63))
                  (((Variable (Generated opt_64)) Pure)
                   ((Variable (Generated opt_65)) Pure))
                  Ctl)
                 Ctl)
                ((Variable (Generated opt_65)) Pure)
                ((Application (Variable (Generated opt_30))
                  (((Variable (User h)) Pure)) Pure)
                 Pure))
               Ctl))))))))))
     ((User main)
      ((((Variable (Generated mon_147)) Pure)) Ctl
       (Let (Variable (Generated mon_149)) Pure
        (Application (Variable (Language perform))
         (((Effect_label console) Pure)
          ((Lambda
            ((((Variable (Generated mon_148)) Pure)) Pure
             (Select_operation console (User println-int)
              (Variable (Generated mon_148)))))
           Pure))
         Pure)
        (Let (Variable (Generated mon_166)) Pure
         (Let (Variable (Generated mon_150)) Pure (Variable (User foo))
          (Let (Variable (Generated mon_154)) Pure
           (Lambda
            ((((Variable (User y)) Pure) ((Variable (Generated mon_151)) Pure))
             Ctl
             (Let (Variable (Generated mon_152)) Pure (Variable (User y))
              (Let (Variable (Generated mon_153)) Pure (Variable (User y))
               (Construct_pure
                (Operator (Variable (Generated mon_152)) (Int Times)
                 (Variable (Generated mon_153))))))))
           (Let (Variable (Generated mon_158)) Pure
            (Lambda
             ((((Variable (User x)) Pure)
               ((Variable (Generated mon_155)) Pure))
              Ctl
              (Let (Variable (Generated mon_156)) Pure (Variable (User x))
               (Let (Variable (Generated mon_157)) Pure (Variable (User x))
                (Construct_pure
                 (Operator (Variable (Generated mon_156)) (Int Plus)
                  (Variable (Generated mon_157))))))))
            (Let (Variable (Generated mon_163)) Pure
             (Lambda
              ((((Variable (User m)) Pure)
                ((Variable (Generated mon_159)) Pure))
               Ctl
               (Construct_pure
                (Lambda
                 ((((Variable (User n)) Pure)
                   ((Variable (Generated mon_160)) Pure))
                  Ctl
                  (Let (Variable (Generated mon_161)) Pure (Variable (User m))
                   (Let (Variable (Generated mon_162)) Pure (Variable (User n))
                    (Construct_pure
                     (Operator (Variable (Generated mon_161)) (Int Minus)
                      (Variable (Generated mon_162)))))))))))
             (Let (Variable (Generated mon_164)) Pure (Literal (Int 3))
              (Match_ctl_pure
               (subject
                (Application (Variable (Generated mon_150))
                 (((Variable (Generated mon_154)) Pure)
                  ((Variable (Generated mon_158)) Pure)
                  ((Variable (Generated mon_163)) Pure)
                  ((Variable (Generated mon_164)) Pure)
                  ((Variable (Generated mon_147)) Pure))
                 Ctl))
               (pure_branch
                ((Variable (Generated mon_165)) (Variable (Generated mon_165))))))))))
         (Application (Variable (Generated mon_149))
          (((Variable (Generated mon_166)) Pure)
           ((Variable (Generated mon_147)) Pure))
          Ctl)))))
     ((Generated opt_66)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_176)) Pure)
          ((Variable (Generated mon_177)) Pure))
         Ctl (Construct_pure (Tuple_construction ()))))))
     ((Language main)
      ((((Variable (Generated mon_167)) Pure)) Ctl
       (Match_ctl
        (subject
         (Let (Variable (Generated mon_174)) Pure
          (Application (Variable (Language handler))
           (((Effect_label console) Pure)
            ((Construct_handler (handled_effect console)
              (operation_clauses
               (((User print-int)
                 (Construct_op_tail
                  (Lambda
                   ((((Variable (Language x)) Pure)
                     ((Variable (Generated mon_168)) Pure))
                    Ctl
                    (Let (Variable (Generated mon_169)) Pure
                     (Variable (Language x))
                     (Construct_pure
                      (Impure_built_in
                       (Impure_print_int (value (Variable (Generated mon_169)))
                        (newline false)))))))))
                ((User println)
                 (Construct_op_tail
                  (Lambda
                   (((Wildcard Pure) ((Variable (Generated mon_170)) Pure)) Ctl
                    (Construct_pure (Impure_built_in Impure_println))))))
                ((User println-int)
                 (Construct_op_tail
                  (Lambda
                   ((((Variable (Language x)) Pure)
                     ((Variable (Generated mon_171)) Pure))
                    Ctl
                    (Let (Variable (Generated mon_172)) Pure
                     (Variable (Language x))
                     (Construct_pure
                      (Impure_built_in
                       (Impure_print_int (value (Variable (Generated mon_172)))
                        (newline true)))))))))
                ((User read-int)
                 (Construct_op_tail
                  (Lambda
                   (((Wildcard Pure) ((Variable (Generated mon_173)) Pure)) Ctl
                    (Construct_pure (Impure_built_in Impure_read_int)))))))))
             Pure))
           Pure)
          (Let (Variable (Generated mon_175)) Pure (Variable (User main))
           (Application (Variable (Generated mon_174))
            (((Variable (Generated mon_175)) Pure)
             ((Variable (Generated mon_167)) Pure))
            Ctl))))
        (pure_branch
         ((Variable (Generated mon_176))
          (Let (Variable (Generated mon_177)) Pure
           (Variable (Generated mon_167))
           (Construct_pure (Tuple_construction ())))))
        (yield_branch
         ((Generated opt_67) (Generated opt_68) (Generated opt_69)
          (Construct_yield (marker (Variable (Generated opt_67)))
           (op_clause (Variable (Generated opt_68)))
           (resumption
            (Lambda
             ((((Variable (Generated opt_70)) Pure)
               ((Variable (Generated opt_71)) Pure))
              Ctl
              (Application (Variable (Language bind))
               (((Application (Variable (Generated opt_69))
                  (((Variable (Generated opt_70)) Pure)
                   ((Variable (Generated opt_71)) Pure))
                  Ctl)
                 Ctl)
                ((Variable (Generated opt_71)) Pure)
                ((Application (Variable (Generated opt_66)) () Pure) Pure))
               Ctl))))))))))))
   (entry_expr
    (Application (Variable (Language main)) ((Nil_evidence_vector Pure)) Ctl)))
