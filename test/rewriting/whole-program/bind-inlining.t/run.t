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
     ((User max)
      ((((Variable (User x)) Pure) ((Variable (User y)) Pure)
        ((Variable (Generated mon_45)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (Generated mon_48)) Pure
         (Let (Variable (Generated mon_46)) Pure (Variable (User x))
          (Let (Variable (Generated mon_47)) Pure (Variable (User y))
           (Operator (Variable (Generated mon_46)) (Int Greater_equal)
            (Variable (Generated mon_47)))))
         (If_then_else (Variable (Generated mon_48)) (Variable (User x))
          (Variable (User y)))))))
     ((User reverse)
      ((((Variable (User xs)) Pure) ((Variable (Generated mon_49)) Pure)) Ctl
       (Construct_pure
        (Let (Variable (User reverse-tail)) Pure
         (Fix_lambda
          ((User reverse-tail)
           ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
             ((Variable (Generated mon_50)) Pure))
            Ctl
            (Construct_pure
             (Let (Variable (Generated mon_51)) Pure (Variable (User xs))
              (Match (Variable (Generated mon_51)) List
               (((Construction List_nil ()) (Variable (User acc)))
                ((Construction List_cons
                  ((Variable (User x)) (Variable (User xx))))
                 (Let (Variable (Generated mon_52)) Pure
                  (Variable (User reverse-tail))
                  (Let (Variable (Generated mon_53)) Pure (Variable (User xx))
                   (Let (Variable (Generated mon_56)) Pure
                    (Let (Variable (Generated mon_54)) Pure (Variable (User x))
                     (Let (Variable (Generated mon_55)) Pure
                      (Variable (User acc))
                      (Construction List_cons
                       ((Variable (Generated mon_54))
                        (Variable (Generated mon_55))))))
                    (Match_ctl_pure
                     (subject
                      (Application (Variable (Generated mon_52))
                       (((Variable (Generated mon_53)) Pure)
                        ((Variable (Generated mon_56)) Pure)
                        ((Variable (Generated mon_50)) Pure))
                       Ctl))
                     (pure_branch
                      ((Variable (Generated mon_57))
                       (Variable (Generated mon_57))))))))))))))))
         (Let (Variable (Generated mon_58)) Pure (Variable (User reverse-tail))
          (Let (Variable (Generated mon_59)) Pure (Variable (User xs))
           (Let (Variable (Generated mon_60)) Pure (Construction List_nil ())
            (Match_ctl_pure
             (subject
              (Application (Variable (Generated mon_58))
               (((Variable (Generated mon_59)) Pure)
                ((Variable (Generated mon_60)) Pure)
                ((Variable (Generated mon_49)) Pure))
               Ctl))
             (pure_branch
              ((Variable (Generated mon_61)) (Variable (Generated mon_61))))))))))))
     ((User concat)
      ((((Variable (User xs)) Pure) ((Variable (User ys)) Pure)
        ((Variable (Generated mon_62)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (User concat-rev)) Pure
         (Fix_lambda
          ((User concat-rev)
           ((((Variable (User rev-xs)) Pure) ((Variable (User acc)) Pure)
             ((Variable (Generated mon_63)) Pure))
            Ctl
            (Construct_pure
             (Let (Variable (Generated mon_64)) Pure (Variable (User rev-xs))
              (Match (Variable (Generated mon_64)) List
               (((Construction List_nil ()) (Variable (User acc)))
                ((Construction List_cons
                  ((Variable (User x)) (Variable (User xx))))
                 (Let (Variable (Generated mon_65)) Pure
                  (Variable (User concat-rev))
                  (Let (Variable (Generated mon_66)) Pure (Variable (User xx))
                   (Let (Variable (Generated mon_69)) Pure
                    (Let (Variable (Generated mon_67)) Pure (Variable (User x))
                     (Let (Variable (Generated mon_68)) Pure
                      (Variable (User acc))
                      (Construction List_cons
                       ((Variable (Generated mon_67))
                        (Variable (Generated mon_68))))))
                    (Match_ctl_pure
                     (subject
                      (Application (Variable (Generated mon_65))
                       (((Variable (Generated mon_66)) Pure)
                        ((Variable (Generated mon_69)) Pure)
                        ((Variable (Generated mon_63)) Pure))
                       Ctl))
                     (pure_branch
                      ((Variable (Generated mon_70))
                       (Variable (Generated mon_70))))))))))))))))
         (Let (Variable (Generated mon_71)) Pure (Variable (User concat-rev))
          (Let (Variable (Generated mon_75)) Pure
           (Let (Variable (Generated mon_72)) Pure (Variable (User reverse))
            (Let (Variable (Generated mon_73)) Pure (Variable (User xs))
             (Match_ctl_pure
              (subject
               (Application (Variable (Generated mon_72))
                (((Variable (Generated mon_73)) Pure)
                 ((Variable (Generated mon_62)) Pure))
                Ctl))
              (pure_branch
               ((Variable (Generated mon_74)) (Variable (Generated mon_74)))))))
           (Let (Variable (Generated mon_76)) Pure (Variable (User ys))
            (Match_ctl_pure
             (subject
              (Application (Variable (Generated mon_71))
               (((Variable (Generated mon_75)) Pure)
                ((Variable (Generated mon_76)) Pure)
                ((Variable (Generated mon_62)) Pure))
               Ctl))
             (pure_branch
              ((Variable (Generated mon_77)) (Variable (Generated mon_77))))))))))))
     ((User map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_78)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (User map-tail)) Pure
         (Fix_lambda
          ((User map-tail)
           ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
             ((Variable (User f)) Pure) ((Variable (Generated mon_79)) Pure))
            Ctl
            (Let (Variable (Generated mon_80)) Pure (Variable (User xs))
             (Match (Variable (Generated mon_80)) List
              (((Construction List_nil ())
                (Construct_pure
                 (Let (Variable (Generated mon_81)) Pure
                  (Variable (User reverse))
                  (Let (Variable (Generated mon_82)) Pure (Variable (User acc))
                   (Match_ctl_pure
                    (subject
                     (Application (Variable (Generated mon_81))
                      (((Variable (Generated mon_82)) Pure)
                       ((Variable (Generated mon_79)) Pure))
                      Ctl))
                    (pure_branch
                     ((Variable (Generated mon_83))
                      (Variable (Generated mon_83)))))))))
               ((Construction List_cons
                 ((Variable (User y)) (Variable (User ys))))
                (Let (Variable (Generated mon_84)) Pure
                 (Variable (User map-tail))
                 (Let (Variable (Generated mon_85)) Pure (Variable (User ys))
                  (Application (Variable (Language bind))
                   (((Application (Variable (Language bind))
                      (((Let (Variable (Generated mon_86)) Pure
                         (Variable (User f))
                         (Let (Variable (Generated mon_87)) Pure
                          (Variable (User y))
                          (Application (Variable (Generated mon_86))
                           (((Variable (Generated mon_87)) Pure)
                            ((Variable (Generated mon_79)) Pure))
                           Ctl)))
                        Ctl)
                       ((Variable (Generated mon_79)) Pure)
                       ((Lambda
                         ((((Variable (Generated mon_88)) Pure)
                           ((Variable (Generated mon_89)) Pure))
                          Ctl
                          (Construct_pure
                           (Let (Variable (Generated mon_90)) Pure
                            (Variable (User acc))
                            (Construction List_cons
                             ((Variable (Generated mon_88))
                              (Variable (Generated mon_90))))))))
                        Pure))
                      Ctl)
                     Ctl)
                    ((Variable (Generated mon_79)) Pure)
                    ((Lambda
                      ((((Variable (Generated mon_91)) Pure)
                        ((Variable (Generated mon_92)) Pure))
                       Ctl
                       (Construct_pure
                        (Let (Variable (Generated mon_93)) Pure
                         (Variable (User f))
                         (Match_ctl_pure
                          (subject
                           (Application (Variable (Generated mon_84))
                            (((Variable (Generated mon_85)) Pure)
                             ((Variable (Generated mon_91)) Pure)
                             ((Variable (Generated mon_93)) Pure)
                             ((Variable (Generated mon_92)) Pure))
                            Ctl))
                          (pure_branch
                           ((Variable (Generated mon_94))
                            (Variable (Generated mon_94)))))))))
                     Pure))
                   Ctl))))))))))
         (Let (Variable (Generated mon_95)) Pure (Variable (User map-tail))
          (Let (Variable (Generated mon_96)) Pure (Variable (User xs))
           (Let (Variable (Generated mon_97)) Pure (Construction List_nil ())
            (Let (Variable (Generated mon_98)) Pure (Variable (User f))
             (Match_ctl_pure
              (subject
               (Application (Variable (Generated mon_95))
                (((Variable (Generated mon_96)) Pure)
                 ((Variable (Generated mon_97)) Pure)
                 ((Variable (Generated mon_98)) Pure)
                 ((Variable (Generated mon_78)) Pure))
                Ctl))
              (pure_branch
               ((Variable (Generated mon_99)) (Variable (Generated mon_99)))))))))))))
     ((User exists)
      ((((Variable (User xs)) Pure) ((Variable (User p)) Pure)
        ((Variable (Generated mon_100)) Pure))
       Ctl
       (Let (Variable (Generated mon_101)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_101)) List
         (((Construction List_nil ()) (Construct_pure (Literal (Bool false))))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Application (Variable (Language bind))
            (((Let (Variable (Generated mon_102)) Pure (Variable (User p))
               (Let (Variable (Generated mon_103)) Pure (Variable (User y))
                (Application (Variable (Generated mon_102))
                 (((Variable (Generated mon_103)) Pure)
                  ((Variable (Generated mon_100)) Pure))
                 Ctl)))
              Ctl)
             ((Variable (Generated mon_100)) Pure)
             ((Lambda
               ((((Variable (Generated mon_104)) Pure)
                 ((Variable (Generated mon_105)) Pure))
                Ctl
                (Construct_pure
                 (If_then_else (Variable (Generated mon_104))
                  (Literal (Bool true))
                  (Let (Variable (Generated mon_106)) Pure
                   (Variable (User exists))
                   (Let (Variable (Generated mon_107)) Pure
                    (Variable (User ys))
                    (Let (Variable (Generated mon_108)) Pure
                     (Variable (User p))
                     (Match_ctl_pure
                      (subject
                       (Application (Variable (Generated mon_106))
                        (((Variable (Generated mon_107)) Pure)
                         ((Variable (Generated mon_108)) Pure)
                         ((Variable (Generated mon_105)) Pure))
                        Ctl))
                      (pure_branch
                       ((Variable (Generated mon_109))
                        (Variable (Generated mon_109))))))))))))
              Pure))
            Ctl)))))))
     ((User for-all)
      ((((Variable (User xs)) Pure) ((Variable (User p)) Pure)
        ((Variable (Generated mon_110)) Pure))
       Ctl
       (Let (Variable (Generated mon_111)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_111)) List
         (((Construction List_nil ()) (Construct_pure (Literal (Bool true))))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Application (Variable (Language bind))
            (((Let (Variable (Generated mon_112)) Pure (Variable (User p))
               (Let (Variable (Generated mon_113)) Pure (Variable (User y))
                (Application (Variable (Generated mon_112))
                 (((Variable (Generated mon_113)) Pure)
                  ((Variable (Generated mon_110)) Pure))
                 Ctl)))
              Ctl)
             ((Variable (Generated mon_110)) Pure)
             ((Lambda
               ((((Variable (Generated mon_114)) Pure)
                 ((Variable (Generated mon_115)) Pure))
                Ctl
                (Construct_pure
                 (If_then_else (Variable (Generated mon_114))
                  (Let (Variable (Generated mon_116)) Pure
                   (Variable (User for-all))
                   (Let (Variable (Generated mon_117)) Pure
                    (Variable (User ys))
                    (Let (Variable (Generated mon_118)) Pure
                     (Variable (User p))
                     (Match_ctl_pure
                      (subject
                       (Application (Variable (Generated mon_116))
                        (((Variable (Generated mon_117)) Pure)
                         ((Variable (Generated mon_118)) Pure)
                         ((Variable (Generated mon_115)) Pure))
                        Ctl))
                      (pure_branch
                       ((Variable (Generated mon_119))
                        (Variable (Generated mon_119))))))))
                  (Literal (Bool false))))))
              Pure))
            Ctl)))))))
     ((User list-equal)
      ((((Variable (User xs)) Pure) ((Variable (User ys)) Pure)
        ((Variable (User element-equal)) Pure)
        ((Variable (Generated mon_120)) Pure))
       Ctl
       (Let (Variable (Generated mon_121)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_121)) List
         (((Construction List_nil ())
           (Construct_pure
            (Let (Variable (Generated mon_122)) Pure (Variable (User ys))
             (Match (Variable (Generated mon_122)) List
              (((Construction List_nil ()) (Literal (Bool true)))
               ((Construction List_cons (Wildcard Wildcard))
                (Literal (Bool false))))))))
          ((Construction List_cons ((Variable (User x)) (Variable (User xx))))
           (Let (Variable (Generated mon_123)) Pure (Variable (User ys))
            (Match (Variable (Generated mon_123)) List
             (((Construction List_nil ())
               (Construct_pure (Literal (Bool false))))
              ((Construction List_cons
                ((Variable (User y)) (Variable (User yy))))
               (Application (Variable (Language bind))
                (((Let (Variable (Generated mon_124)) Pure
                   (Variable (User element-equal))
                   (Let (Variable (Generated mon_125)) Pure (Variable (User x))
                    (Let (Variable (Generated mon_126)) Pure
                     (Variable (User y))
                     (Application (Variable (Generated mon_124))
                      (((Variable (Generated mon_125)) Pure)
                       ((Variable (Generated mon_126)) Pure)
                       ((Variable (Generated mon_120)) Pure))
                      Ctl))))
                  Ctl)
                 ((Variable (Generated mon_120)) Pure)
                 ((Lambda
                   ((((Variable (Generated mon_127)) Pure)
                     ((Variable (Generated mon_128)) Pure))
                    Ctl
                    (Construct_pure
                     (If_then_else (Variable (Generated mon_127))
                      (Let (Variable (Generated mon_129)) Pure
                       (Variable (User list-equal))
                       (Let (Variable (Generated mon_130)) Pure
                        (Variable (User xx))
                        (Let (Variable (Generated mon_131)) Pure
                         (Variable (User yy))
                         (Let (Variable (Generated mon_132)) Pure
                          (Variable (User element-equal))
                          (Match_ctl_pure
                           (subject
                            (Application (Variable (Generated mon_129))
                             (((Variable (Generated mon_130)) Pure)
                              ((Variable (Generated mon_131)) Pure)
                              ((Variable (Generated mon_132)) Pure)
                              ((Variable (Generated mon_128)) Pure))
                             Ctl))
                           (pure_branch
                            ((Variable (Generated mon_133))
                             (Variable (Generated mon_133)))))))))
                      (Literal (Bool false))))))
                  Pure))
                Ctl)))))))))))
     ((User filter)
      ((((Variable (User xs)) Pure) ((Variable (User p)) Pure)
        ((Variable (Generated mon_134)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (User filter-tail)) Pure
         (Fix_lambda
          ((User filter-tail)
           ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
             ((Variable (Generated mon_135)) Pure))
            Ctl
            (Let (Variable (Generated mon_136)) Pure (Variable (User xs))
             (Match (Variable (Generated mon_136)) List
              (((Construction List_nil ())
                (Construct_pure
                 (Let (Variable (Generated mon_137)) Pure
                  (Variable (User reverse))
                  (Let (Variable (Generated mon_138)) Pure
                   (Variable (User acc))
                   (Match_ctl_pure
                    (subject
                     (Application (Variable (Generated mon_137))
                      (((Variable (Generated mon_138)) Pure)
                       ((Variable (Generated mon_135)) Pure))
                      Ctl))
                    (pure_branch
                     ((Variable (Generated mon_139))
                      (Variable (Generated mon_139)))))))))
               ((Construction List_cons
                 ((Variable (User y)) (Variable (User ys))))
                (Application (Variable (Language bind))
                 (((Let (Variable (Generated mon_140)) Pure (Variable (User p))
                    (Let (Variable (Generated mon_141)) Pure
                     (Variable (User y))
                     (Application (Variable (Generated mon_140))
                      (((Variable (Generated mon_141)) Pure)
                       ((Variable (Generated mon_135)) Pure))
                      Ctl)))
                   Ctl)
                  ((Variable (Generated mon_135)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_142)) Pure)
                      ((Variable (Generated mon_143)) Pure))
                     Ctl
                     (Construct_pure
                      (If_then_else (Variable (Generated mon_142))
                       (Let (Variable (Generated mon_144)) Pure
                        (Variable (User filter-tail))
                        (Let (Variable (Generated mon_145)) Pure
                         (Variable (User ys))
                         (Let (Variable (Generated mon_148)) Pure
                          (Let (Variable (Generated mon_146)) Pure
                           (Variable (User y))
                           (Let (Variable (Generated mon_147)) Pure
                            (Variable (User acc))
                            (Construction List_cons
                             ((Variable (Generated mon_146))
                              (Variable (Generated mon_147))))))
                          (Match_ctl_pure
                           (subject
                            (Application (Variable (Generated mon_144))
                             (((Variable (Generated mon_145)) Pure)
                              ((Variable (Generated mon_148)) Pure)
                              ((Variable (Generated mon_143)) Pure))
                             Ctl))
                           (pure_branch
                            ((Variable (Generated mon_149))
                             (Variable (Generated mon_149))))))))
                       (Let (Variable (Generated mon_150)) Pure
                        (Variable (User filter-tail))
                        (Let (Variable (Generated mon_151)) Pure
                         (Variable (User ys))
                         (Let (Variable (Generated mon_152)) Pure
                          (Variable (User acc))
                          (Match_ctl_pure
                           (subject
                            (Application (Variable (Generated mon_150))
                             (((Variable (Generated mon_151)) Pure)
                              ((Variable (Generated mon_152)) Pure)
                              ((Variable (Generated mon_143)) Pure))
                             Ctl))
                           (pure_branch
                            ((Variable (Generated mon_153))
                             (Variable (Generated mon_153))))))))))))
                   Pure))
                 Ctl))))))))
         (Let (Variable (Generated mon_154)) Pure (Variable (User filter-tail))
          (Let (Variable (Generated mon_155)) Pure (Variable (User xs))
           (Let (Variable (Generated mon_156)) Pure (Construction List_nil ())
            (Match_ctl_pure
             (subject
              (Application (Variable (Generated mon_154))
               (((Variable (Generated mon_155)) Pure)
                ((Variable (Generated mon_156)) Pure)
                ((Variable (Generated mon_134)) Pure))
               Ctl))
             (pure_branch
              ((Variable (Generated mon_157)) (Variable (Generated mon_157))))))))))))
     ((User filter-map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_158)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (User filter-map-tail)) Pure
         (Fix_lambda
          ((User filter-map-tail)
           ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
             ((Variable (Generated mon_159)) Pure))
            Ctl
            (Let (Variable (Generated mon_160)) Pure (Variable (User xs))
             (Match (Variable (Generated mon_160)) List
              (((Construction List_nil ())
                (Construct_pure
                 (Let (Variable (Generated mon_161)) Pure
                  (Variable (User reverse))
                  (Let (Variable (Generated mon_162)) Pure
                   (Variable (User acc))
                   (Match_ctl_pure
                    (subject
                     (Application (Variable (Generated mon_161))
                      (((Variable (Generated mon_162)) Pure)
                       ((Variable (Generated mon_159)) Pure))
                      Ctl))
                    (pure_branch
                     ((Variable (Generated mon_163))
                      (Variable (Generated mon_163)))))))))
               ((Construction List_cons
                 ((Variable (User y)) (Variable (User ys))))
                (Application (Variable (Language bind))
                 (((Let (Variable (Generated mon_164)) Pure (Variable (User f))
                    (Let (Variable (Generated mon_165)) Pure
                     (Variable (User y))
                     (Application (Variable (Generated mon_164))
                      (((Variable (Generated mon_165)) Pure)
                       ((Variable (Generated mon_159)) Pure))
                      Ctl)))
                   Ctl)
                  ((Variable (Generated mon_159)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_166)) Pure)
                      ((Variable (Generated mon_167)) Pure))
                     Ctl
                     (Construct_pure
                      (Match (Variable (Generated mon_166)) Option
                       (((Construction Option_none ())
                         (Let (Variable (Generated mon_168)) Pure
                          (Variable (User filter-map-tail))
                          (Let (Variable (Generated mon_169)) Pure
                           (Variable (User ys))
                           (Let (Variable (Generated mon_170)) Pure
                            (Variable (User acc))
                            (Match_ctl_pure
                             (subject
                              (Application (Variable (Generated mon_168))
                               (((Variable (Generated mon_169)) Pure)
                                ((Variable (Generated mon_170)) Pure)
                                ((Variable (Generated mon_167)) Pure))
                               Ctl))
                             (pure_branch
                              ((Variable (Generated mon_171))
                               (Variable (Generated mon_171)))))))))
                        ((Construction Option_some ((Variable (User z))))
                         (Let (Variable (Generated mon_172)) Pure
                          (Variable (User filter-map-tail))
                          (Let (Variable (Generated mon_173)) Pure
                           (Variable (User ys))
                           (Let (Variable (Generated mon_176)) Pure
                            (Let (Variable (Generated mon_174)) Pure
                             (Variable (User z))
                             (Let (Variable (Generated mon_175)) Pure
                              (Variable (User acc))
                              (Construction List_cons
                               ((Variable (Generated mon_174))
                                (Variable (Generated mon_175))))))
                            (Match_ctl_pure
                             (subject
                              (Application (Variable (Generated mon_172))
                               (((Variable (Generated mon_173)) Pure)
                                ((Variable (Generated mon_176)) Pure)
                                ((Variable (Generated mon_167)) Pure))
                               Ctl))
                             (pure_branch
                              ((Variable (Generated mon_177))
                               (Variable (Generated mon_177))))))))))))))
                   Pure))
                 Ctl))))))))
         (Let (Variable (Generated mon_178)) Pure
          (Variable (User filter-map-tail))
          (Let (Variable (Generated mon_179)) Pure (Variable (User xs))
           (Let (Variable (Generated mon_180)) Pure (Construction List_nil ())
            (Match_ctl_pure
             (subject
              (Application (Variable (Generated mon_178))
               (((Variable (Generated mon_179)) Pure)
                ((Variable (Generated mon_180)) Pure)
                ((Variable (Generated mon_158)) Pure))
               Ctl))
             (pure_branch
              ((Variable (Generated mon_181)) (Variable (Generated mon_181))))))))))))
     ((User flat-map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_182)) Pure))
       Ctl
       (Let (Variable (Generated mon_183)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_183)) List
         (((Construction List_nil ())
           (Construct_pure (Construction List_nil ())))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Let (Variable (Generated mon_184)) Pure (Variable (User concat))
            (Application (Variable (Language bind))
             (((Let (Variable (Generated mon_185)) Pure (Variable (User f))
                (Let (Variable (Generated mon_186)) Pure (Variable (User y))
                 (Application (Variable (Generated mon_185))
                  (((Variable (Generated mon_186)) Pure)
                   ((Variable (Generated mon_182)) Pure))
                  Ctl)))
               Ctl)
              ((Variable (Generated mon_182)) Pure)
              ((Lambda
                ((((Variable (Generated mon_187)) Pure)
                  ((Variable (Generated mon_188)) Pure))
                 Ctl
                 (Construct_pure
                  (Let (Variable (Generated mon_193)) Pure
                   (Let (Variable (Generated mon_189)) Pure
                    (Variable (User flat-map))
                    (Let (Variable (Generated mon_190)) Pure
                     (Variable (User ys))
                     (Let (Variable (Generated mon_191)) Pure
                      (Variable (User f))
                      (Match_ctl_pure
                       (subject
                        (Application (Variable (Generated mon_189))
                         (((Variable (Generated mon_190)) Pure)
                          ((Variable (Generated mon_191)) Pure)
                          ((Variable (Generated mon_188)) Pure))
                         Ctl))
                       (pure_branch
                        ((Variable (Generated mon_192))
                         (Variable (Generated mon_192))))))))
                   (Match_ctl_pure
                    (subject
                     (Application (Variable (Generated mon_184))
                      (((Variable (Generated mon_187)) Pure)
                       ((Variable (Generated mon_193)) Pure)
                       ((Variable (Generated mon_188)) Pure))
                      Ctl))
                    (pure_branch
                     ((Variable (Generated mon_194))
                      (Variable (Generated mon_194)))))))))
               Pure))
             Ctl))))))))
     ((User fold)
      ((((Variable (User xs)) Pure) ((Variable (User init)) Pure)
        ((Variable (User f)) Pure) ((Variable (Generated mon_195)) Pure))
       Ctl
       (Let (Variable (Generated mon_196)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_196)) List
         (((Construction List_nil ()) (Construct_pure (Variable (User init))))
          ((Construction List_cons ((Variable (User x)) (Variable (User ys))))
           (Let (Variable (Generated mon_197)) Pure (Variable (User fold))
            (Let (Variable (Generated mon_198)) Pure (Variable (User ys))
             (Application (Variable (Language bind))
              (((Let (Variable (Generated mon_199)) Pure (Variable (User f))
                 (Let (Variable (Generated mon_200)) Pure
                  (Variable (User init))
                  (Let (Variable (Generated mon_201)) Pure (Variable (User x))
                   (Application (Variable (Generated mon_199))
                    (((Variable (Generated mon_200)) Pure)
                     ((Variable (Generated mon_201)) Pure)
                     ((Variable (Generated mon_195)) Pure))
                    Ctl))))
                Ctl)
               ((Variable (Generated mon_195)) Pure)
               ((Lambda
                 ((((Variable (Generated mon_202)) Pure)
                   ((Variable (Generated mon_203)) Pure))
                  Ctl
                  (Construct_pure
                   (Let (Variable (Generated mon_204)) Pure (Variable (User f))
                    (Match_ctl_pure
                     (subject
                      (Application (Variable (Generated mon_197))
                       (((Variable (Generated mon_198)) Pure)
                        ((Variable (Generated mon_202)) Pure)
                        ((Variable (Generated mon_204)) Pure)
                        ((Variable (Generated mon_203)) Pure))
                       Ctl))
                     (pure_branch
                      ((Variable (Generated mon_205))
                       (Variable (Generated mon_205)))))))))
                Pure))
              Ctl)))))))))
     ((User reduce)
      ((((Variable (User xs)) Pure) ((Variable (User combine)) Pure)
        ((Variable (Generated mon_206)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (Generated mon_207)) Pure (Variable (User xs))
         (Match (Variable (Generated mon_207)) List
          (((Construction List_nil ()) (Construction Option_none ()))
           ((Construction List_cons ((Variable (User x)) (Variable (User ys))))
            (Let (Variable (Generated mon_208)) Pure (Variable (User fold))
             (Let (Variable (Generated mon_209)) Pure (Variable (User ys))
              (Let (Variable (Generated mon_210)) Pure (Variable (User x))
               (Let (Variable (Generated mon_211)) Pure
                (Variable (User combine))
                (Match_ctl_pure
                 (subject
                  (Application (Variable (Generated mon_208))
                   (((Variable (Generated mon_209)) Pure)
                    ((Variable (Generated mon_210)) Pure)
                    ((Variable (Generated mon_211)) Pure)
                    ((Variable (Generated mon_206)) Pure))
                   Ctl))
                 (pure_branch
                  ((Variable (Generated mon_212))
                   (Variable (Generated mon_212))))))))))))))))
     ((User for-each)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_213)) Pure))
       Ctl
       (Let (Variable (Generated mon_214)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_214)) List
         (((Construction List_nil ()) (Construct_pure (Tuple_construction ())))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Application (Variable (Language bind))
            (((Let (Variable (Generated mon_215)) Pure (Variable (User f))
               (Let (Variable (Generated mon_216)) Pure (Variable (User y))
                (Application (Variable (Generated mon_215))
                 (((Variable (Generated mon_216)) Pure)
                  ((Variable (Generated mon_213)) Pure))
                 Ctl)))
              Ctl)
             ((Variable (Generated mon_213)) Pure)
             ((Lambda
               ((((Variable (Generated mon_217)) Pure)
                 ((Variable (Generated mon_218)) Pure))
                Ctl
                (Construct_pure
                 (Let (Variable (Generated mon_219)) Pure
                  (Variable (User for-each))
                  (Let (Variable (Generated mon_220)) Pure (Variable (User ys))
                   (Let (Variable (Generated mon_221)) Pure (Variable (User f))
                    (Match_ctl_pure
                     (subject
                      (Application (Variable (Generated mon_219))
                       (((Variable (Generated mon_220)) Pure)
                        ((Variable (Generated mon_221)) Pure)
                        ((Variable (Generated mon_218)) Pure))
                       Ctl))
                     (pure_branch
                      ((Variable (Generated mon_222))
                       (Variable (Generated mon_222)))))))))))
              Pure))
            Ctl)))))))
     ((User range)
      ((((Variable (User start)) Pure) ((Variable (User stop)) Pure)
        ((Variable (Generated mon_223)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (Generated mon_226)) Pure
         (Let (Variable (Generated mon_224)) Pure (Variable (User start))
          (Let (Variable (Generated mon_225)) Pure (Variable (User stop))
           (Operator (Variable (Generated mon_224)) (Int Greater_equal)
            (Variable (Generated mon_225)))))
         (If_then_else (Variable (Generated mon_226))
          (Construction List_nil ())
          (Let (Variable (Generated mon_227)) Pure (Variable (User start))
           (Let (Variable (Generated mon_234)) Pure
            (Let (Variable (Generated mon_228)) Pure (Variable (User range))
             (Let (Variable (Generated mon_231)) Pure
              (Let (Variable (Generated mon_229)) Pure (Variable (User start))
               (Let (Variable (Generated mon_230)) Pure (Literal (Int 1))
                (Operator (Variable (Generated mon_229)) (Int Plus)
                 (Variable (Generated mon_230)))))
              (Let (Variable (Generated mon_232)) Pure (Variable (User stop))
               (Match_ctl_pure
                (subject
                 (Application (Variable (Generated mon_228))
                  (((Variable (Generated mon_231)) Pure)
                   ((Variable (Generated mon_232)) Pure)
                   ((Variable (Generated mon_223)) Pure))
                  Ctl))
                (pure_branch
                 ((Variable (Generated mon_233))
                  (Variable (Generated mon_233))))))))
            (Construction List_cons
             ((Variable (Generated mon_227)) (Variable (Generated mon_234)))))))))))
     ((User print-list)
      ((((Variable (User xs)) Pure) ((Variable (User print-element)) Pure)
        ((Variable (Generated mon_235)) Pure))
       Ctl
       (Let (Variable (Generated mon_240)) Pure
        (Let (Variable (Generated mon_236)) Pure (Variable (User for-each))
         (Let (Variable (Generated mon_237)) Pure (Variable (User xs))
          (Let (Variable (Generated mon_238)) Pure
           (Variable (User print-element))
           (Match_ctl_pure
            (subject
             (Application (Variable (Generated mon_236))
              (((Variable (Generated mon_237)) Pure)
               ((Variable (Generated mon_238)) Pure)
               ((Variable (Generated mon_235)) Pure))
              Ctl))
            (pure_branch
             ((Variable (Generated mon_239)) (Variable (Generated mon_239))))))))
        (Let (Variable (Generated mon_242)) Pure
         (Application (Variable (Language perform))
          (((Effect_label console) Pure)
           ((Lambda
             ((((Variable (Generated mon_241)) Pure)) Pure
              (Select_operation console (User println)
               (Variable (Generated mon_241)))))
            Pure))
          Pure)
         (Let (Variable (Generated mon_243)) Pure (Tuple_construction ())
          (Application (Variable (Generated mon_242))
           (((Variable (Generated mon_243)) Pure)
            ((Variable (Generated mon_235)) Pure))
           Ctl))))))
     ((User head)
      ((((Variable (User xs)) Pure) ((Variable (Generated mon_244)) Pure)) Ctl
       (Construct_pure
        (Let (Variable (Generated mon_245)) Pure (Variable (User xs))
         (Match (Variable (Generated mon_245)) List
          (((Construction List_nil ()) (Construction Option_none ()))
           ((Construction List_cons ((Variable (User x)) Wildcard))
            (Let (Variable (Generated mon_246)) Pure (Variable (User x))
             (Construction Option_some ((Variable (Generated mon_246))))))))))))
     ((User some-if)
      ((((Variable (User cond)) Pure) ((Variable (User value)) Pure)
        ((Variable (Generated mon_247)) Pure))
       Ctl
       (Construct_pure
        (Let (Variable (Generated mon_248)) Pure (Variable (User cond))
         (If_then_else (Variable (Generated mon_248))
          (Let (Variable (Generated mon_249)) Pure (Variable (User value))
           (Construction Option_some ((Variable (Generated mon_249)))))
          (Construction Option_none ()))))))
     ((User option-map)
      ((((Variable (User x)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_250)) Pure))
       Ctl
       (Let (Variable (Generated mon_251)) Pure (Variable (User x))
        (Match (Variable (Generated mon_251)) Option
         (((Construction Option_none ())
           (Construct_pure (Construction Option_none ())))
          ((Construction Option_some ((Variable (User y))))
           (Application (Variable (Language bind))
            (((Let (Variable (Generated mon_252)) Pure (Variable (User f))
               (Let (Variable (Generated mon_253)) Pure (Variable (User y))
                (Application (Variable (Generated mon_252))
                 (((Variable (Generated mon_253)) Pure)
                  ((Variable (Generated mon_250)) Pure))
                 Ctl)))
              Ctl)
             ((Variable (Generated mon_250)) Pure)
             ((Lambda
               ((((Variable (Generated mon_254)) Pure)
                 ((Variable (Generated mon_255)) Pure))
                Ctl
                (Construct_pure
                 (Construction Option_some ((Variable (Generated mon_254)))))))
              Pure))
            Ctl)))))))
     ((User option-flat-map)
      ((((Variable (User x)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_256)) Pure))
       Ctl
       (Let (Variable (Generated mon_257)) Pure (Variable (User x))
        (Match (Variable (Generated mon_257)) Option
         (((Construction Option_none ())
           (Construct_pure (Construction Option_none ())))
          ((Construction Option_some ((Variable (User y))))
           (Let (Variable (Generated mon_258)) Pure (Variable (User f))
            (Let (Variable (Generated mon_259)) Pure (Variable (User y))
             (Application (Variable (Generated mon_258))
              (((Variable (Generated mon_259)) Pure)
               ((Variable (Generated mon_256)) Pure))
              Ctl)))))))))
     ((User tuple2-equal)
      ((((Tuple ((Variable (User a1)) (Variable (User a2)))) Pure)
        ((Tuple ((Variable (User b1)) (Variable (User b2)))) Pure)
        ((Variable (User equal1)) Pure) ((Variable (User equal2)) Pure)
        ((Variable (Generated mon_260)) Pure))
       Ctl
       (Application (Variable (Language bind))
        (((Let (Variable (Generated mon_261)) Pure (Variable (User equal1))
           (Let (Variable (Generated mon_262)) Pure (Variable (User a1))
            (Let (Variable (Generated mon_263)) Pure (Variable (User b1))
             (Application (Variable (Generated mon_261))
              (((Variable (Generated mon_262)) Pure)
               ((Variable (Generated mon_263)) Pure)
               ((Variable (Generated mon_260)) Pure))
              Ctl))))
          Ctl)
         ((Variable (Generated mon_260)) Pure)
         ((Lambda
           ((((Variable (Generated mon_264)) Pure)
             ((Variable (Generated mon_265)) Pure))
            Ctl
            (If_then_else (Variable (Generated mon_264))
             (Let (Variable (Generated mon_266)) Pure (Variable (User equal2))
              (Let (Variable (Generated mon_267)) Pure (Variable (User a2))
               (Let (Variable (Generated mon_268)) Pure (Variable (User b2))
                (Application (Variable (Generated mon_266))
                 (((Variable (Generated mon_267)) Pure)
                  ((Variable (Generated mon_268)) Pure)
                  ((Variable (Generated mon_265)) Pure))
                 Ctl))))
             (Construct_pure (Literal (Bool false))))))
          Pure))
        Ctl)))
     ((User foo)
      ((((Variable (User f)) Pure) ((Variable (User g)) Pure)
        ((Variable (User h)) Pure) ((Variable (User x)) Pure)
        ((Variable (Generated mon_269)) Pure))
       Ctl
       (Application (Variable (Language bind))
        (((Application (Variable (Language bind))
           (((Application (Variable (Language bind))
              (((Let (Variable (Generated mon_270)) Pure (Variable (User f))
                 (Application (Variable (Language bind))
                  (((Let (Variable (Generated mon_271)) Pure
                     (Variable (User g))
                     (Let (Variable (Generated mon_272)) Pure
                      (Variable (User x))
                      (Application (Variable (Generated mon_271))
                       (((Variable (Generated mon_272)) Pure)
                        ((Variable (Generated mon_269)) Pure))
                       Ctl)))
                    Ctl)
                   ((Variable (Generated mon_269)) Pure)
                   ((Lambda
                     ((((Variable (Generated mon_273)) Pure)
                       ((Variable (Generated mon_274)) Pure))
                      Ctl
                      (Application (Variable (Generated mon_270))
                       (((Variable (Generated mon_273)) Pure)
                        ((Variable (Generated mon_274)) Pure))
                       Ctl)))
                    Pure))
                  Ctl))
                Ctl)
               ((Variable (Generated mon_269)) Pure)
               ((Lambda
                 ((((Variable (Generated mon_275)) Pure)
                   ((Variable (Generated mon_276)) Pure))
                  Ctl
                  (Construct_pure
                   (Let (Variable (Generated mon_277)) Pure (Literal (Int 0))
                    (Operator (Variable (Generated mon_275)) (Int Greater_than)
                     (Variable (Generated mon_277)))))))
                Pure))
              Ctl)
             Ctl)
            ((Variable (Generated mon_269)) Pure)
            ((Lambda
              ((((Variable (Generated mon_278)) Pure)
                ((Variable (Generated mon_279)) Pure))
               Ctl
               (If_then_else (Variable (Generated mon_278))
                (Application (Variable (Language bind))
                 (((Let (Variable (Generated mon_280)) Pure (Variable (User h))
                    (Let (Variable (Generated mon_281)) Pure (Literal (Int 0))
                     (Application (Variable (Generated mon_280))
                      (((Variable (Generated mon_281)) Pure)
                       ((Variable (Generated mon_279)) Pure))
                      Ctl)))
                   Ctl)
                  ((Variable (Generated mon_279)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_282)) Pure)
                      ((Variable (Generated mon_283)) Pure))
                     Ctl
                     (Let (Variable (Generated mon_284)) Pure (Literal (Int 1))
                      (Application (Variable (Generated mon_282))
                       (((Variable (Generated mon_284)) Pure)
                        ((Variable (Generated mon_283)) Pure))
                       Ctl))))
                   Pure))
                 Ctl)
                (Application (Variable (Language bind))
                 (((Let (Variable (Generated mon_285)) Pure (Variable (User h))
                    (Let (Variable (Generated mon_286)) Pure (Literal (Int 1))
                     (Application (Variable (Generated mon_285))
                      (((Variable (Generated mon_286)) Pure)
                       ((Variable (Generated mon_279)) Pure))
                      Ctl)))
                   Ctl)
                  ((Variable (Generated mon_279)) Pure)
                  ((Lambda
                    ((((Variable (Generated mon_287)) Pure)
                      ((Variable (Generated mon_288)) Pure))
                     Ctl
                     (Let (Variable (Generated mon_289)) Pure (Literal (Int 0))
                      (Application (Variable (Generated mon_287))
                       (((Variable (Generated mon_289)) Pure)
                        ((Variable (Generated mon_288)) Pure))
                       Ctl))))
                   Pure))
                 Ctl))))
             Pure))
           Ctl)
          Ctl)
         ((Variable (Generated mon_269)) Pure)
         ((Lambda
           ((((Variable (Generated mon_290)) Pure)
             ((Variable (Generated mon_291)) Pure))
            Ctl
            (Let (Variable (User z)) Pure (Variable (Generated mon_290))
             (Application (Variable (Language bind))
              (((Let (Variable (Generated mon_292)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_293)) Pure (Variable (User z))
                  (Application (Variable (Generated mon_292))
                   (((Variable (Generated mon_293)) Pure)
                    ((Variable (Generated mon_291)) Pure))
                   Ctl)))
                Ctl)
               ((Variable (Generated mon_291)) Pure)
               ((Lambda
                 ((((Variable (Generated mon_294)) Pure)
                   ((Variable (Generated mon_295)) Pure))
                  Ctl
                  (Let (Variable (Generated mon_296)) Pure (Variable (User z))
                   (Application (Variable (Generated mon_294))
                    (((Variable (Generated mon_296)) Pure)
                     ((Variable (Generated mon_295)) Pure))
                    Ctl))))
                Pure))
              Ctl))))
          Pure))
        Ctl)))
     ((User main)
      ((((Variable (Generated mon_297)) Pure)) Ctl
       (Let (Variable (Generated mon_299)) Pure
        (Application (Variable (Language perform))
         (((Effect_label console) Pure)
          ((Lambda
            ((((Variable (Generated mon_298)) Pure)) Pure
             (Select_operation console (User println-int)
              (Variable (Generated mon_298)))))
           Pure))
         Pure)
        (Let (Variable (Generated mon_316)) Pure
         (Let (Variable (Generated mon_300)) Pure (Variable (User foo))
          (Let (Variable (Generated mon_304)) Pure
           (Lambda
            ((((Variable (User y)) Pure) ((Variable (Generated mon_301)) Pure))
             Ctl
             (Construct_pure
              (Let (Variable (Generated mon_302)) Pure (Variable (User y))
               (Let (Variable (Generated mon_303)) Pure (Variable (User y))
                (Operator (Variable (Generated mon_302)) (Int Times)
                 (Variable (Generated mon_303))))))))
           (Let (Variable (Generated mon_308)) Pure
            (Lambda
             ((((Variable (User x)) Pure)
               ((Variable (Generated mon_305)) Pure))
              Ctl
              (Construct_pure
               (Let (Variable (Generated mon_306)) Pure (Variable (User x))
                (Let (Variable (Generated mon_307)) Pure (Variable (User x))
                 (Operator (Variable (Generated mon_306)) (Int Plus)
                  (Variable (Generated mon_307))))))))
            (Let (Variable (Generated mon_313)) Pure
             (Lambda
              ((((Variable (User m)) Pure)
                ((Variable (Generated mon_309)) Pure))
               Ctl
               (Construct_pure
                (Lambda
                 ((((Variable (User n)) Pure)
                   ((Variable (Generated mon_310)) Pure))
                  Ctl
                  (Construct_pure
                   (Let (Variable (Generated mon_311)) Pure (Variable (User m))
                    (Let (Variable (Generated mon_312)) Pure
                     (Variable (User n))
                     (Operator (Variable (Generated mon_311)) (Int Minus)
                      (Variable (Generated mon_312)))))))))))
             (Let (Variable (Generated mon_314)) Pure (Literal (Int 3))
              (Match_ctl_pure
               (subject
                (Application (Variable (Generated mon_300))
                 (((Variable (Generated mon_304)) Pure)
                  ((Variable (Generated mon_308)) Pure)
                  ((Variable (Generated mon_313)) Pure)
                  ((Variable (Generated mon_314)) Pure)
                  ((Variable (Generated mon_297)) Pure))
                 Ctl))
               (pure_branch
                ((Variable (Generated mon_315)) (Variable (Generated mon_315))))))))))
         (Application (Variable (Generated mon_299))
          (((Variable (Generated mon_316)) Pure)
           ((Variable (Generated mon_297)) Pure))
          Ctl)))))
     ((Language main)
      ((((Variable (Generated mon_317)) Pure)) Ctl
       (Application (Variable (Language bind))
        (((Let (Variable (Generated mon_324)) Pure
           (Application (Variable (Language handler))
            (((Effect_label console) Pure)
             ((Construct_handler (handled_effect console)
               (operation_clauses
                (((User print-int)
                  (Construct_op_tail
                   (Lambda
                    ((((Variable (Language x)) Pure)
                      ((Variable (Generated mon_318)) Pure))
                     Ctl
                     (Construct_pure
                      (Let (Variable (Generated mon_319)) Pure
                       (Variable (Language x))
                       (Impure_built_in
                        (Impure_print_int
                         (value (Variable (Generated mon_319)))
                         (newline false)))))))))
                 ((User println)
                  (Construct_op_tail
                   (Lambda
                    (((Wildcard Pure) ((Variable (Generated mon_320)) Pure))
                     Ctl (Construct_pure (Impure_built_in Impure_println))))))
                 ((User println-int)
                  (Construct_op_tail
                   (Lambda
                    ((((Variable (Language x)) Pure)
                      ((Variable (Generated mon_321)) Pure))
                     Ctl
                     (Construct_pure
                      (Let (Variable (Generated mon_322)) Pure
                       (Variable (Language x))
                       (Impure_built_in
                        (Impure_print_int
                         (value (Variable (Generated mon_322))) (newline true)))))))))
                 ((User read-int)
                  (Construct_op_tail
                   (Lambda
                    (((Wildcard Pure) ((Variable (Generated mon_323)) Pure))
                     Ctl (Construct_pure (Impure_built_in Impure_read_int)))))))))
              Pure))
            Pure)
           (Let (Variable (Generated mon_325)) Pure (Variable (User main))
            (Application (Variable (Generated mon_324))
             (((Variable (Generated mon_325)) Pure)
              ((Variable (Generated mon_317)) Pure))
             Ctl)))
          Ctl)
         ((Variable (Generated mon_317)) Pure)
         ((Lambda
           ((((Variable (Generated mon_326)) Pure)
             ((Variable (Generated mon_327)) Pure))
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
     ((User max)
      ((((Variable (User x)) Pure) ((Variable (User y)) Pure)
        ((Variable (Generated mon_45)) Pure))
       Ctl
       (Let (Variable (Generated mon_48)) Pure
        (Let (Variable (Generated mon_46)) Pure (Variable (User x))
         (Let (Variable (Generated mon_47)) Pure (Variable (User y))
          (Operator (Variable (Generated mon_46)) (Int Greater_equal)
           (Variable (Generated mon_47)))))
        (If_then_else (Variable (Generated mon_48))
         (Construct_pure (Variable (User x)))
         (Construct_pure (Variable (User y)))))))
     ((User reverse)
      ((((Variable (User xs)) Pure) ((Variable (Generated mon_49)) Pure)) Ctl
       (Let (Variable (User reverse-tail)) Pure
        (Fix_lambda
         ((User reverse-tail)
          ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
            ((Variable (Generated mon_50)) Pure))
           Ctl
           (Let (Variable (Generated mon_51)) Pure (Variable (User xs))
            (Match (Variable (Generated mon_51)) List
             (((Construction List_nil ())
               (Construct_pure (Variable (User acc))))
              ((Construction List_cons
                ((Variable (User x)) (Variable (User xx))))
               (Let (Variable (Generated mon_52)) Pure
                (Variable (User reverse-tail))
                (Let (Variable (Generated mon_53)) Pure (Variable (User xx))
                 (Let (Variable (Generated mon_56)) Pure
                  (Let (Variable (Generated mon_54)) Pure (Variable (User x))
                   (Let (Variable (Generated mon_55)) Pure
                    (Variable (User acc))
                    (Construction List_cons
                     ((Variable (Generated mon_54))
                      (Variable (Generated mon_55))))))
                  (Application (Variable (Generated mon_52))
                   (((Variable (Generated mon_53)) Pure)
                    ((Variable (Generated mon_56)) Pure)
                    ((Variable (Generated mon_50)) Pure))
                   Ctl)))))))))))
        (Let (Variable (Generated mon_58)) Pure (Variable (User reverse-tail))
         (Let (Variable (Generated mon_59)) Pure (Variable (User xs))
          (Let (Variable (Generated mon_60)) Pure (Construction List_nil ())
           (Application (Variable (Generated mon_58))
            (((Variable (Generated mon_59)) Pure)
             ((Variable (Generated mon_60)) Pure)
             ((Variable (Generated mon_49)) Pure))
            Ctl)))))))
     ((User concat)
      ((((Variable (User xs)) Pure) ((Variable (User ys)) Pure)
        ((Variable (Generated mon_62)) Pure))
       Ctl
       (Let (Variable (User concat-rev)) Pure
        (Fix_lambda
         ((User concat-rev)
          ((((Variable (User rev-xs)) Pure) ((Variable (User acc)) Pure)
            ((Variable (Generated mon_63)) Pure))
           Ctl
           (Let (Variable (Generated mon_64)) Pure (Variable (User rev-xs))
            (Match (Variable (Generated mon_64)) List
             (((Construction List_nil ())
               (Construct_pure (Variable (User acc))))
              ((Construction List_cons
                ((Variable (User x)) (Variable (User xx))))
               (Let (Variable (Generated mon_65)) Pure
                (Variable (User concat-rev))
                (Let (Variable (Generated mon_66)) Pure (Variable (User xx))
                 (Let (Variable (Generated mon_69)) Pure
                  (Let (Variable (Generated mon_67)) Pure (Variable (User x))
                   (Let (Variable (Generated mon_68)) Pure
                    (Variable (User acc))
                    (Construction List_cons
                     ((Variable (Generated mon_67))
                      (Variable (Generated mon_68))))))
                  (Application (Variable (Generated mon_65))
                   (((Variable (Generated mon_66)) Pure)
                    ((Variable (Generated mon_69)) Pure)
                    ((Variable (Generated mon_63)) Pure))
                   Ctl)))))))))))
        (Let (Variable (Generated mon_71)) Pure (Variable (User concat-rev))
         (Let (Variable (Generated mon_75)) Pure
          (Let (Variable (Generated mon_72)) Pure (Variable (User reverse))
           (Let (Variable (Generated mon_73)) Pure (Variable (User xs))
            (Match_ctl_pure
             (subject
              (Application (Variable (Generated mon_72))
               (((Variable (Generated mon_73)) Pure)
                ((Variable (Generated mon_62)) Pure))
               Ctl))
             (pure_branch
              ((Variable (Generated mon_74)) (Variable (Generated mon_74)))))))
          (Let (Variable (Generated mon_76)) Pure (Variable (User ys))
           (Application (Variable (Generated mon_71))
            (((Variable (Generated mon_75)) Pure)
             ((Variable (Generated mon_76)) Pure)
             ((Variable (Generated mon_62)) Pure))
            Ctl)))))))
     ((Generated opt_1)
      ((((Variable (User acc)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_88)) Pure)
          ((Variable (Generated mon_89)) Pure))
         Ctl
         (Let (Variable (Generated mon_90)) Pure (Variable (User acc))
          (Construct_pure
           (Construction List_cons
            ((Variable (Generated mon_88)) (Variable (Generated mon_90))))))))))
     ((Generated opt_0)
      ((((Variable (User f)) Pure) ((Variable (Generated mon_84)) Pure)
        ((Variable (Generated mon_85)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_91)) Pure)
          ((Variable (Generated mon_92)) Pure))
         Ctl
         (Let (Variable (Generated mon_93)) Pure (Variable (User f))
          (Application (Variable (Generated mon_84))
           (((Variable (Generated mon_85)) Pure)
            ((Variable (Generated mon_91)) Pure)
            ((Variable (Generated mon_93)) Pure)
            ((Variable (Generated mon_92)) Pure))
           Ctl))))))
     ((User map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_78)) Pure))
       Ctl
       (Let (Variable (User map-tail)) Pure
        (Fix_lambda
         ((User map-tail)
          ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
            ((Variable (User f)) Pure) ((Variable (Generated mon_79)) Pure))
           Ctl
           (Let (Variable (Generated mon_80)) Pure (Variable (User xs))
            (Match (Variable (Generated mon_80)) List
             (((Construction List_nil ())
               (Let (Variable (Generated mon_81)) Pure
                (Variable (User reverse))
                (Let (Variable (Generated mon_82)) Pure (Variable (User acc))
                 (Application (Variable (Generated mon_81))
                  (((Variable (Generated mon_82)) Pure)
                   ((Variable (Generated mon_79)) Pure))
                  Ctl))))
              ((Construction List_cons
                ((Variable (User y)) (Variable (User ys))))
               (Let (Variable (Generated mon_84)) Pure
                (Variable (User map-tail))
                (Let (Variable (Generated mon_85)) Pure (Variable (User ys))
                 (Match_ctl
                  (subject
                   (Match_ctl
                    (subject
                     (Let (Variable (Generated mon_86)) Pure
                      (Variable (User f))
                      (Let (Variable (Generated mon_87)) Pure
                       (Variable (User y))
                       (Application (Variable (Generated mon_86))
                        (((Variable (Generated mon_87)) Pure)
                         ((Variable (Generated mon_79)) Pure))
                        Ctl))))
                    (pure_branch
                     ((Variable (Generated mon_88))
                      (Let (Variable (Generated mon_89)) Pure
                       (Variable (Generated mon_79))
                       (Let (Variable (Generated mon_90)) Pure
                        (Variable (User acc))
                        (Construct_pure
                         (Construction List_cons
                          ((Variable (Generated mon_88))
                           (Variable (Generated mon_90)))))))))
                    (yield_branch
                     ((Generated opt_2) (Generated opt_3) (Generated opt_4)
                      (Construct_yield (marker (Variable (Generated opt_2)))
                       (op_clause (Variable (Generated opt_3)))
                       (resumption
                        (Lambda
                         ((((Variable (Generated opt_5)) Pure)
                           ((Variable (Generated opt_6)) Pure))
                          Ctl
                          (Application (Variable (Language bind))
                           (((Application (Variable (Generated opt_4))
                              (((Variable (Generated opt_5)) Pure)
                               ((Variable (Generated opt_6)) Pure))
                              Ctl)
                             Ctl)
                            ((Variable (Generated opt_6)) Pure)
                            ((Application (Variable (Generated opt_1))
                              (((Variable (User acc)) Pure)) Pure)
                             Pure))
                           Ctl)))))))))
                  (pure_branch
                   ((Variable (Generated mon_91))
                    (Let (Variable (Generated mon_92)) Pure
                     (Variable (Generated mon_79))
                     (Let (Variable (Generated mon_93)) Pure
                      (Variable (User f))
                      (Application (Variable (Generated mon_84))
                       (((Variable (Generated mon_85)) Pure)
                        ((Variable (Generated mon_91)) Pure)
                        ((Variable (Generated mon_93)) Pure)
                        ((Variable (Generated mon_92)) Pure))
                       Ctl)))))
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
                          ((Application (Variable (Generated opt_0))
                            (((Variable (User f)) Pure)
                             ((Variable (Generated mon_84)) Pure)
                             ((Variable (Generated mon_85)) Pure))
                            Pure)
                           Pure))
                         Ctl)))))))))))))))))
        (Let (Variable (Generated mon_95)) Pure (Variable (User map-tail))
         (Let (Variable (Generated mon_96)) Pure (Variable (User xs))
          (Let (Variable (Generated mon_97)) Pure (Construction List_nil ())
           (Let (Variable (Generated mon_98)) Pure (Variable (User f))
            (Application (Variable (Generated mon_95))
             (((Variable (Generated mon_96)) Pure)
              ((Variable (Generated mon_97)) Pure)
              ((Variable (Generated mon_98)) Pure)
              ((Variable (Generated mon_78)) Pure))
             Ctl))))))))
     ((Generated opt_12)
      ((((Variable (User exists)) Pure) ((Variable (User p)) Pure)
        ((Variable (User ys)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_104)) Pure)
          ((Variable (Generated mon_105)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_104))
          (Construct_pure (Literal (Bool true)))
          (Let (Variable (Generated mon_106)) Pure (Variable (User exists))
           (Let (Variable (Generated mon_107)) Pure (Variable (User ys))
            (Let (Variable (Generated mon_108)) Pure (Variable (User p))
             (Application (Variable (Generated mon_106))
              (((Variable (Generated mon_107)) Pure)
               ((Variable (Generated mon_108)) Pure)
               ((Variable (Generated mon_105)) Pure))
              Ctl)))))))))
     ((User exists)
      ((((Variable (User xs)) Pure) ((Variable (User p)) Pure)
        ((Variable (Generated mon_100)) Pure))
       Ctl
       (Let (Variable (Generated mon_101)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_101)) List
         (((Construction List_nil ()) (Construct_pure (Literal (Bool false))))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_102)) Pure (Variable (User p))
              (Let (Variable (Generated mon_103)) Pure (Variable (User y))
               (Application (Variable (Generated mon_102))
                (((Variable (Generated mon_103)) Pure)
                 ((Variable (Generated mon_100)) Pure))
                Ctl))))
            (pure_branch
             ((Variable (Generated mon_104))
              (Let (Variable (Generated mon_105)) Pure
               (Variable (Generated mon_100))
               (If_then_else (Variable (Generated mon_104))
                (Construct_pure (Literal (Bool true)))
                (Let (Variable (Generated mon_106)) Pure
                 (Variable (User exists))
                 (Let (Variable (Generated mon_107)) Pure (Variable (User ys))
                  (Let (Variable (Generated mon_108)) Pure (Variable (User p))
                   (Application (Variable (Generated mon_106))
                    (((Variable (Generated mon_107)) Pure)
                     ((Variable (Generated mon_108)) Pure)
                     ((Variable (Generated mon_105)) Pure))
                    Ctl))))))))
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
                    ((Application (Variable (Generated opt_12))
                      (((Variable (User exists)) Pure)
                       ((Variable (User p)) Pure) ((Variable (User ys)) Pure))
                      Pure)
                     Pure))
                   Ctl))))))))))))))
     ((Generated opt_18)
      ((((Variable (User for-all)) Pure) ((Variable (User p)) Pure)
        ((Variable (User ys)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_114)) Pure)
          ((Variable (Generated mon_115)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_114))
          (Let (Variable (Generated mon_116)) Pure (Variable (User for-all))
           (Let (Variable (Generated mon_117)) Pure (Variable (User ys))
            (Let (Variable (Generated mon_118)) Pure (Variable (User p))
             (Application (Variable (Generated mon_116))
              (((Variable (Generated mon_117)) Pure)
               ((Variable (Generated mon_118)) Pure)
               ((Variable (Generated mon_115)) Pure))
              Ctl))))
          (Construct_pure (Literal (Bool false))))))))
     ((User for-all)
      ((((Variable (User xs)) Pure) ((Variable (User p)) Pure)
        ((Variable (Generated mon_110)) Pure))
       Ctl
       (Let (Variable (Generated mon_111)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_111)) List
         (((Construction List_nil ()) (Construct_pure (Literal (Bool true))))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_112)) Pure (Variable (User p))
              (Let (Variable (Generated mon_113)) Pure (Variable (User y))
               (Application (Variable (Generated mon_112))
                (((Variable (Generated mon_113)) Pure)
                 ((Variable (Generated mon_110)) Pure))
                Ctl))))
            (pure_branch
             ((Variable (Generated mon_114))
              (Let (Variable (Generated mon_115)) Pure
               (Variable (Generated mon_110))
               (If_then_else (Variable (Generated mon_114))
                (Let (Variable (Generated mon_116)) Pure
                 (Variable (User for-all))
                 (Let (Variable (Generated mon_117)) Pure (Variable (User ys))
                  (Let (Variable (Generated mon_118)) Pure (Variable (User p))
                   (Application (Variable (Generated mon_116))
                    (((Variable (Generated mon_117)) Pure)
                     ((Variable (Generated mon_118)) Pure)
                     ((Variable (Generated mon_115)) Pure))
                    Ctl))))
                (Construct_pure (Literal (Bool false)))))))
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
                      (((Variable (User for-all)) Pure)
                       ((Variable (User p)) Pure) ((Variable (User ys)) Pure))
                      Pure)
                     Pure))
                   Ctl))))))))))))))
     ((Generated opt_24)
      ((((Variable (User element-equal)) Pure)
        ((Variable (User list-equal)) Pure) ((Variable (User xx)) Pure)
        ((Variable (User yy)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_127)) Pure)
          ((Variable (Generated mon_128)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_127))
          (Let (Variable (Generated mon_129)) Pure (Variable (User list-equal))
           (Let (Variable (Generated mon_130)) Pure (Variable (User xx))
            (Let (Variable (Generated mon_131)) Pure (Variable (User yy))
             (Let (Variable (Generated mon_132)) Pure
              (Variable (User element-equal))
              (Application (Variable (Generated mon_129))
               (((Variable (Generated mon_130)) Pure)
                ((Variable (Generated mon_131)) Pure)
                ((Variable (Generated mon_132)) Pure)
                ((Variable (Generated mon_128)) Pure))
               Ctl)))))
          (Construct_pure (Literal (Bool false))))))))
     ((User list-equal)
      ((((Variable (User xs)) Pure) ((Variable (User ys)) Pure)
        ((Variable (User element-equal)) Pure)
        ((Variable (Generated mon_120)) Pure))
       Ctl
       (Let (Variable (Generated mon_121)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_121)) List
         (((Construction List_nil ())
           (Let (Variable (Generated mon_122)) Pure (Variable (User ys))
            (Match (Variable (Generated mon_122)) List
             (((Construction List_nil ())
               (Construct_pure (Literal (Bool true))))
              ((Construction List_cons (Wildcard Wildcard))
               (Construct_pure (Literal (Bool false))))))))
          ((Construction List_cons ((Variable (User x)) (Variable (User xx))))
           (Let (Variable (Generated mon_123)) Pure (Variable (User ys))
            (Match (Variable (Generated mon_123)) List
             (((Construction List_nil ())
               (Construct_pure (Literal (Bool false))))
              ((Construction List_cons
                ((Variable (User y)) (Variable (User yy))))
               (Match_ctl
                (subject
                 (Let (Variable (Generated mon_124)) Pure
                  (Variable (User element-equal))
                  (Let (Variable (Generated mon_125)) Pure (Variable (User x))
                   (Let (Variable (Generated mon_126)) Pure (Variable (User y))
                    (Application (Variable (Generated mon_124))
                     (((Variable (Generated mon_125)) Pure)
                      ((Variable (Generated mon_126)) Pure)
                      ((Variable (Generated mon_120)) Pure))
                     Ctl)))))
                (pure_branch
                 ((Variable (Generated mon_127))
                  (Let (Variable (Generated mon_128)) Pure
                   (Variable (Generated mon_120))
                   (If_then_else (Variable (Generated mon_127))
                    (Let (Variable (Generated mon_129)) Pure
                     (Variable (User list-equal))
                     (Let (Variable (Generated mon_130)) Pure
                      (Variable (User xx))
                      (Let (Variable (Generated mon_131)) Pure
                       (Variable (User yy))
                       (Let (Variable (Generated mon_132)) Pure
                        (Variable (User element-equal))
                        (Application (Variable (Generated mon_129))
                         (((Variable (Generated mon_130)) Pure)
                          ((Variable (Generated mon_131)) Pure)
                          ((Variable (Generated mon_132)) Pure)
                          ((Variable (Generated mon_128)) Pure))
                         Ctl)))))
                    (Construct_pure (Literal (Bool false)))))))
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
                          (((Variable (User element-equal)) Pure)
                           ((Variable (User list-equal)) Pure)
                           ((Variable (User xx)) Pure)
                           ((Variable (User yy)) Pure))
                          Pure)
                         Pure))
                       Ctl))))))))))))))))))
     ((Generated opt_30)
      ((((Variable (User acc)) Pure) ((Variable (User filter-tail)) Pure)
        ((Variable (User y)) Pure) ((Variable (User ys)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_142)) Pure)
          ((Variable (Generated mon_143)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_142))
          (Let (Variable (Generated mon_144)) Pure
           (Variable (User filter-tail))
           (Let (Variable (Generated mon_145)) Pure (Variable (User ys))
            (Let (Variable (Generated mon_148)) Pure
             (Let (Variable (Generated mon_146)) Pure (Variable (User y))
              (Let (Variable (Generated mon_147)) Pure (Variable (User acc))
               (Construction List_cons
                ((Variable (Generated mon_146)) (Variable (Generated mon_147))))))
             (Application (Variable (Generated mon_144))
              (((Variable (Generated mon_145)) Pure)
               ((Variable (Generated mon_148)) Pure)
               ((Variable (Generated mon_143)) Pure))
              Ctl))))
          (Let (Variable (Generated mon_150)) Pure
           (Variable (User filter-tail))
           (Let (Variable (Generated mon_151)) Pure (Variable (User ys))
            (Let (Variable (Generated mon_152)) Pure (Variable (User acc))
             (Application (Variable (Generated mon_150))
              (((Variable (Generated mon_151)) Pure)
               ((Variable (Generated mon_152)) Pure)
               ((Variable (Generated mon_143)) Pure))
              Ctl)))))))))
     ((User filter)
      ((((Variable (User xs)) Pure) ((Variable (User p)) Pure)
        ((Variable (Generated mon_134)) Pure))
       Ctl
       (Let (Variable (User filter-tail)) Pure
        (Fix_lambda
         ((User filter-tail)
          ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
            ((Variable (Generated mon_135)) Pure))
           Ctl
           (Let (Variable (Generated mon_136)) Pure (Variable (User xs))
            (Match (Variable (Generated mon_136)) List
             (((Construction List_nil ())
               (Let (Variable (Generated mon_137)) Pure
                (Variable (User reverse))
                (Let (Variable (Generated mon_138)) Pure (Variable (User acc))
                 (Application (Variable (Generated mon_137))
                  (((Variable (Generated mon_138)) Pure)
                   ((Variable (Generated mon_135)) Pure))
                  Ctl))))
              ((Construction List_cons
                ((Variable (User y)) (Variable (User ys))))
               (Match_ctl
                (subject
                 (Let (Variable (Generated mon_140)) Pure (Variable (User p))
                  (Let (Variable (Generated mon_141)) Pure (Variable (User y))
                   (Application (Variable (Generated mon_140))
                    (((Variable (Generated mon_141)) Pure)
                     ((Variable (Generated mon_135)) Pure))
                    Ctl))))
                (pure_branch
                 ((Variable (Generated mon_142))
                  (Let (Variable (Generated mon_143)) Pure
                   (Variable (Generated mon_135))
                   (If_then_else (Variable (Generated mon_142))
                    (Let (Variable (Generated mon_144)) Pure
                     (Variable (User filter-tail))
                     (Let (Variable (Generated mon_145)) Pure
                      (Variable (User ys))
                      (Let (Variable (Generated mon_148)) Pure
                       (Let (Variable (Generated mon_146)) Pure
                        (Variable (User y))
                        (Let (Variable (Generated mon_147)) Pure
                         (Variable (User acc))
                         (Construction List_cons
                          ((Variable (Generated mon_146))
                           (Variable (Generated mon_147))))))
                       (Application (Variable (Generated mon_144))
                        (((Variable (Generated mon_145)) Pure)
                         ((Variable (Generated mon_148)) Pure)
                         ((Variable (Generated mon_143)) Pure))
                        Ctl))))
                    (Let (Variable (Generated mon_150)) Pure
                     (Variable (User filter-tail))
                     (Let (Variable (Generated mon_151)) Pure
                      (Variable (User ys))
                      (Let (Variable (Generated mon_152)) Pure
                       (Variable (User acc))
                       (Application (Variable (Generated mon_150))
                        (((Variable (Generated mon_151)) Pure)
                         ((Variable (Generated mon_152)) Pure)
                         ((Variable (Generated mon_143)) Pure))
                        Ctl))))))))
                (yield_branch
                 ((Generated opt_31) (Generated opt_32) (Generated opt_33)
                  (Construct_yield (marker (Variable (Generated opt_31)))
                   (op_clause (Variable (Generated opt_32)))
                   (resumption
                    (Lambda
                     ((((Variable (Generated opt_34)) Pure)
                       ((Variable (Generated opt_35)) Pure))
                      Ctl
                      (Application (Variable (Language bind))
                       (((Application (Variable (Generated opt_33))
                          (((Variable (Generated opt_34)) Pure)
                           ((Variable (Generated opt_35)) Pure))
                          Ctl)
                         Ctl)
                        ((Variable (Generated opt_35)) Pure)
                        ((Application (Variable (Generated opt_30))
                          (((Variable (User acc)) Pure)
                           ((Variable (User filter-tail)) Pure)
                           ((Variable (User y)) Pure)
                           ((Variable (User ys)) Pure))
                          Pure)
                         Pure))
                       Ctl)))))))))))))))
        (Let (Variable (Generated mon_154)) Pure (Variable (User filter-tail))
         (Let (Variable (Generated mon_155)) Pure (Variable (User xs))
          (Let (Variable (Generated mon_156)) Pure (Construction List_nil ())
           (Application (Variable (Generated mon_154))
            (((Variable (Generated mon_155)) Pure)
             ((Variable (Generated mon_156)) Pure)
             ((Variable (Generated mon_134)) Pure))
            Ctl)))))))
     ((Generated opt_36)
      ((((Variable (User acc)) Pure) ((Variable (User filter-map-tail)) Pure)
        ((Variable (User ys)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_166)) Pure)
          ((Variable (Generated mon_167)) Pure))
         Ctl
         (Match (Variable (Generated mon_166)) Option
          (((Construction Option_none ())
            (Let (Variable (Generated mon_168)) Pure
             (Variable (User filter-map-tail))
             (Let (Variable (Generated mon_169)) Pure (Variable (User ys))
              (Let (Variable (Generated mon_170)) Pure (Variable (User acc))
               (Application (Variable (Generated mon_168))
                (((Variable (Generated mon_169)) Pure)
                 ((Variable (Generated mon_170)) Pure)
                 ((Variable (Generated mon_167)) Pure))
                Ctl)))))
           ((Construction Option_some ((Variable (User z))))
            (Let (Variable (Generated mon_172)) Pure
             (Variable (User filter-map-tail))
             (Let (Variable (Generated mon_173)) Pure (Variable (User ys))
              (Let (Variable (Generated mon_176)) Pure
               (Let (Variable (Generated mon_174)) Pure (Variable (User z))
                (Let (Variable (Generated mon_175)) Pure (Variable (User acc))
                 (Construction List_cons
                  ((Variable (Generated mon_174))
                   (Variable (Generated mon_175))))))
               (Application (Variable (Generated mon_172))
                (((Variable (Generated mon_173)) Pure)
                 ((Variable (Generated mon_176)) Pure)
                 ((Variable (Generated mon_167)) Pure))
                Ctl)))))))))))
     ((User filter-map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_158)) Pure))
       Ctl
       (Let (Variable (User filter-map-tail)) Pure
        (Fix_lambda
         ((User filter-map-tail)
          ((((Variable (User xs)) Pure) ((Variable (User acc)) Pure)
            ((Variable (Generated mon_159)) Pure))
           Ctl
           (Let (Variable (Generated mon_160)) Pure (Variable (User xs))
            (Match (Variable (Generated mon_160)) List
             (((Construction List_nil ())
               (Let (Variable (Generated mon_161)) Pure
                (Variable (User reverse))
                (Let (Variable (Generated mon_162)) Pure (Variable (User acc))
                 (Application (Variable (Generated mon_161))
                  (((Variable (Generated mon_162)) Pure)
                   ((Variable (Generated mon_159)) Pure))
                  Ctl))))
              ((Construction List_cons
                ((Variable (User y)) (Variable (User ys))))
               (Match_ctl
                (subject
                 (Let (Variable (Generated mon_164)) Pure (Variable (User f))
                  (Let (Variable (Generated mon_165)) Pure (Variable (User y))
                   (Application (Variable (Generated mon_164))
                    (((Variable (Generated mon_165)) Pure)
                     ((Variable (Generated mon_159)) Pure))
                    Ctl))))
                (pure_branch
                 ((Variable (Generated mon_166))
                  (Let (Variable (Generated mon_167)) Pure
                   (Variable (Generated mon_159))
                   (Match (Variable (Generated mon_166)) Option
                    (((Construction Option_none ())
                      (Let (Variable (Generated mon_168)) Pure
                       (Variable (User filter-map-tail))
                       (Let (Variable (Generated mon_169)) Pure
                        (Variable (User ys))
                        (Let (Variable (Generated mon_170)) Pure
                         (Variable (User acc))
                         (Application (Variable (Generated mon_168))
                          (((Variable (Generated mon_169)) Pure)
                           ((Variable (Generated mon_170)) Pure)
                           ((Variable (Generated mon_167)) Pure))
                          Ctl)))))
                     ((Construction Option_some ((Variable (User z))))
                      (Let (Variable (Generated mon_172)) Pure
                       (Variable (User filter-map-tail))
                       (Let (Variable (Generated mon_173)) Pure
                        (Variable (User ys))
                        (Let (Variable (Generated mon_176)) Pure
                         (Let (Variable (Generated mon_174)) Pure
                          (Variable (User z))
                          (Let (Variable (Generated mon_175)) Pure
                           (Variable (User acc))
                           (Construction List_cons
                            ((Variable (Generated mon_174))
                             (Variable (Generated mon_175))))))
                         (Application (Variable (Generated mon_172))
                          (((Variable (Generated mon_173)) Pure)
                           ((Variable (Generated mon_176)) Pure)
                           ((Variable (Generated mon_167)) Pure))
                          Ctl))))))))))
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
                        ((Application (Variable (Generated opt_36))
                          (((Variable (User acc)) Pure)
                           ((Variable (User filter-map-tail)) Pure)
                           ((Variable (User ys)) Pure))
                          Pure)
                         Pure))
                       Ctl)))))))))))))))
        (Let (Variable (Generated mon_178)) Pure
         (Variable (User filter-map-tail))
         (Let (Variable (Generated mon_179)) Pure (Variable (User xs))
          (Let (Variable (Generated mon_180)) Pure (Construction List_nil ())
           (Application (Variable (Generated mon_178))
            (((Variable (Generated mon_179)) Pure)
             ((Variable (Generated mon_180)) Pure)
             ((Variable (Generated mon_158)) Pure))
            Ctl)))))))
     ((Generated opt_42)
      ((((Variable (User f)) Pure) ((Variable (User flat-map)) Pure)
        ((Variable (User ys)) Pure) ((Variable (Generated mon_184)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_187)) Pure)
          ((Variable (Generated mon_188)) Pure))
         Ctl
         (Let (Variable (Generated mon_193)) Pure
          (Let (Variable (Generated mon_189)) Pure (Variable (User flat-map))
           (Let (Variable (Generated mon_190)) Pure (Variable (User ys))
            (Let (Variable (Generated mon_191)) Pure (Variable (User f))
             (Match_ctl_pure
              (subject
               (Application (Variable (Generated mon_189))
                (((Variable (Generated mon_190)) Pure)
                 ((Variable (Generated mon_191)) Pure)
                 ((Variable (Generated mon_188)) Pure))
                Ctl))
              (pure_branch
               ((Variable (Generated mon_192)) (Variable (Generated mon_192))))))))
          (Application (Variable (Generated mon_184))
           (((Variable (Generated mon_187)) Pure)
            ((Variable (Generated mon_193)) Pure)
            ((Variable (Generated mon_188)) Pure))
           Ctl))))))
     ((User flat-map)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_182)) Pure))
       Ctl
       (Let (Variable (Generated mon_183)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_183)) List
         (((Construction List_nil ())
           (Construct_pure (Construction List_nil ())))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Let (Variable (Generated mon_184)) Pure (Variable (User concat))
            (Match_ctl
             (subject
              (Let (Variable (Generated mon_185)) Pure (Variable (User f))
               (Let (Variable (Generated mon_186)) Pure (Variable (User y))
                (Application (Variable (Generated mon_185))
                 (((Variable (Generated mon_186)) Pure)
                  ((Variable (Generated mon_182)) Pure))
                 Ctl))))
             (pure_branch
              ((Variable (Generated mon_187))
               (Let (Variable (Generated mon_188)) Pure
                (Variable (Generated mon_182))
                (Let (Variable (Generated mon_193)) Pure
                 (Let (Variable (Generated mon_189)) Pure
                  (Variable (User flat-map))
                  (Let (Variable (Generated mon_190)) Pure (Variable (User ys))
                   (Let (Variable (Generated mon_191)) Pure (Variable (User f))
                    (Match_ctl_pure
                     (subject
                      (Application (Variable (Generated mon_189))
                       (((Variable (Generated mon_190)) Pure)
                        ((Variable (Generated mon_191)) Pure)
                        ((Variable (Generated mon_188)) Pure))
                       Ctl))
                     (pure_branch
                      ((Variable (Generated mon_192))
                       (Variable (Generated mon_192))))))))
                 (Application (Variable (Generated mon_184))
                  (((Variable (Generated mon_187)) Pure)
                   ((Variable (Generated mon_193)) Pure)
                   ((Variable (Generated mon_188)) Pure))
                  Ctl)))))
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
                     ((Application (Variable (Generated opt_42))
                       (((Variable (User f)) Pure)
                        ((Variable (User flat-map)) Pure)
                        ((Variable (User ys)) Pure)
                        ((Variable (Generated mon_184)) Pure))
                       Pure)
                      Pure))
                    Ctl)))))))))))))))
     ((Generated opt_48)
      ((((Variable (User f)) Pure) ((Variable (Generated mon_197)) Pure)
        ((Variable (Generated mon_198)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_202)) Pure)
          ((Variable (Generated mon_203)) Pure))
         Ctl
         (Let (Variable (Generated mon_204)) Pure (Variable (User f))
          (Application (Variable (Generated mon_197))
           (((Variable (Generated mon_198)) Pure)
            ((Variable (Generated mon_202)) Pure)
            ((Variable (Generated mon_204)) Pure)
            ((Variable (Generated mon_203)) Pure))
           Ctl))))))
     ((User fold)
      ((((Variable (User xs)) Pure) ((Variable (User init)) Pure)
        ((Variable (User f)) Pure) ((Variable (Generated mon_195)) Pure))
       Ctl
       (Let (Variable (Generated mon_196)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_196)) List
         (((Construction List_nil ()) (Construct_pure (Variable (User init))))
          ((Construction List_cons ((Variable (User x)) (Variable (User ys))))
           (Let (Variable (Generated mon_197)) Pure (Variable (User fold))
            (Let (Variable (Generated mon_198)) Pure (Variable (User ys))
             (Match_ctl
              (subject
               (Let (Variable (Generated mon_199)) Pure (Variable (User f))
                (Let (Variable (Generated mon_200)) Pure (Variable (User init))
                 (Let (Variable (Generated mon_201)) Pure (Variable (User x))
                  (Application (Variable (Generated mon_199))
                   (((Variable (Generated mon_200)) Pure)
                    ((Variable (Generated mon_201)) Pure)
                    ((Variable (Generated mon_195)) Pure))
                   Ctl)))))
              (pure_branch
               ((Variable (Generated mon_202))
                (Let (Variable (Generated mon_203)) Pure
                 (Variable (Generated mon_195))
                 (Let (Variable (Generated mon_204)) Pure (Variable (User f))
                  (Application (Variable (Generated mon_197))
                   (((Variable (Generated mon_198)) Pure)
                    ((Variable (Generated mon_202)) Pure)
                    ((Variable (Generated mon_204)) Pure)
                    ((Variable (Generated mon_203)) Pure))
                   Ctl)))))
              (yield_branch
               ((Generated opt_49) (Generated opt_50) (Generated opt_51)
                (Construct_yield (marker (Variable (Generated opt_49)))
                 (op_clause (Variable (Generated opt_50)))
                 (resumption
                  (Lambda
                   ((((Variable (Generated opt_52)) Pure)
                     ((Variable (Generated opt_53)) Pure))
                    Ctl
                    (Application (Variable (Language bind))
                     (((Application (Variable (Generated opt_51))
                        (((Variable (Generated opt_52)) Pure)
                         ((Variable (Generated opt_53)) Pure))
                        Ctl)
                       Ctl)
                      ((Variable (Generated opt_53)) Pure)
                      ((Application (Variable (Generated opt_48))
                        (((Variable (User f)) Pure)
                         ((Variable (Generated mon_197)) Pure)
                         ((Variable (Generated mon_198)) Pure))
                        Pure)
                       Pure))
                     Ctl))))))))))))))))
     ((User reduce)
      ((((Variable (User xs)) Pure) ((Variable (User combine)) Pure)
        ((Variable (Generated mon_206)) Pure))
       Ctl
       (Let (Variable (Generated mon_207)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_207)) List
         (((Construction List_nil ())
           (Construct_pure (Construction Option_none ())))
          ((Construction List_cons ((Variable (User x)) (Variable (User ys))))
           (Let (Variable (Generated mon_208)) Pure (Variable (User fold))
            (Let (Variable (Generated mon_209)) Pure (Variable (User ys))
             (Let (Variable (Generated mon_210)) Pure (Variable (User x))
              (Let (Variable (Generated mon_211)) Pure
               (Variable (User combine))
               (Application (Variable (Generated mon_208))
                (((Variable (Generated mon_209)) Pure)
                 ((Variable (Generated mon_210)) Pure)
                 ((Variable (Generated mon_211)) Pure)
                 ((Variable (Generated mon_206)) Pure))
                Ctl)))))))))))
     ((Generated opt_54)
      ((((Variable (User f)) Pure) ((Variable (User for-each)) Pure)
        ((Variable (User ys)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_217)) Pure)
          ((Variable (Generated mon_218)) Pure))
         Ctl
         (Let (Variable (Generated mon_219)) Pure (Variable (User for-each))
          (Let (Variable (Generated mon_220)) Pure (Variable (User ys))
           (Let (Variable (Generated mon_221)) Pure (Variable (User f))
            (Application (Variable (Generated mon_219))
             (((Variable (Generated mon_220)) Pure)
              ((Variable (Generated mon_221)) Pure)
              ((Variable (Generated mon_218)) Pure))
             Ctl))))))))
     ((User for-each)
      ((((Variable (User xs)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_213)) Pure))
       Ctl
       (Let (Variable (Generated mon_214)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_214)) List
         (((Construction List_nil ()) (Construct_pure (Tuple_construction ())))
          ((Construction List_cons ((Variable (User y)) (Variable (User ys))))
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_215)) Pure (Variable (User f))
              (Let (Variable (Generated mon_216)) Pure (Variable (User y))
               (Application (Variable (Generated mon_215))
                (((Variable (Generated mon_216)) Pure)
                 ((Variable (Generated mon_213)) Pure))
                Ctl))))
            (pure_branch
             ((Variable (Generated mon_217))
              (Let (Variable (Generated mon_218)) Pure
               (Variable (Generated mon_213))
               (Let (Variable (Generated mon_219)) Pure
                (Variable (User for-each))
                (Let (Variable (Generated mon_220)) Pure (Variable (User ys))
                 (Let (Variable (Generated mon_221)) Pure (Variable (User f))
                  (Application (Variable (Generated mon_219))
                   (((Variable (Generated mon_220)) Pure)
                    ((Variable (Generated mon_221)) Pure)
                    ((Variable (Generated mon_218)) Pure))
                   Ctl)))))))
            (yield_branch
             ((Generated opt_55) (Generated opt_56) (Generated opt_57)
              (Construct_yield (marker (Variable (Generated opt_55)))
               (op_clause (Variable (Generated opt_56)))
               (resumption
                (Lambda
                 ((((Variable (Generated opt_58)) Pure)
                   ((Variable (Generated opt_59)) Pure))
                  Ctl
                  (Application (Variable (Language bind))
                   (((Application (Variable (Generated opt_57))
                      (((Variable (Generated opt_58)) Pure)
                       ((Variable (Generated opt_59)) Pure))
                      Ctl)
                     Ctl)
                    ((Variable (Generated opt_59)) Pure)
                    ((Application (Variable (Generated opt_54))
                      (((Variable (User f)) Pure)
                       ((Variable (User for-each)) Pure)
                       ((Variable (User ys)) Pure))
                      Pure)
                     Pure))
                   Ctl))))))))))))))
     ((User range)
      ((((Variable (User start)) Pure) ((Variable (User stop)) Pure)
        ((Variable (Generated mon_223)) Pure))
       Ctl
       (Let (Variable (Generated mon_226)) Pure
        (Let (Variable (Generated mon_224)) Pure (Variable (User start))
         (Let (Variable (Generated mon_225)) Pure (Variable (User stop))
          (Operator (Variable (Generated mon_224)) (Int Greater_equal)
           (Variable (Generated mon_225)))))
        (If_then_else (Variable (Generated mon_226))
         (Construct_pure (Construction List_nil ()))
         (Let (Variable (Generated mon_227)) Pure (Variable (User start))
          (Let (Variable (Generated mon_234)) Pure
           (Let (Variable (Generated mon_228)) Pure (Variable (User range))
            (Let (Variable (Generated mon_231)) Pure
             (Let (Variable (Generated mon_229)) Pure (Variable (User start))
              (Let (Variable (Generated mon_230)) Pure (Literal (Int 1))
               (Operator (Variable (Generated mon_229)) (Int Plus)
                (Variable (Generated mon_230)))))
             (Let (Variable (Generated mon_232)) Pure (Variable (User stop))
              (Match_ctl_pure
               (subject
                (Application (Variable (Generated mon_228))
                 (((Variable (Generated mon_231)) Pure)
                  ((Variable (Generated mon_232)) Pure)
                  ((Variable (Generated mon_223)) Pure))
                 Ctl))
               (pure_branch
                ((Variable (Generated mon_233)) (Variable (Generated mon_233))))))))
           (Construct_pure
            (Construction List_cons
             ((Variable (Generated mon_227)) (Variable (Generated mon_234)))))))))))
     ((User print-list)
      ((((Variable (User xs)) Pure) ((Variable (User print-element)) Pure)
        ((Variable (Generated mon_235)) Pure))
       Ctl
       (Let (Variable (Generated mon_240)) Pure
        (Let (Variable (Generated mon_236)) Pure (Variable (User for-each))
         (Let (Variable (Generated mon_237)) Pure (Variable (User xs))
          (Let (Variable (Generated mon_238)) Pure
           (Variable (User print-element))
           (Match_ctl_pure
            (subject
             (Application (Variable (Generated mon_236))
              (((Variable (Generated mon_237)) Pure)
               ((Variable (Generated mon_238)) Pure)
               ((Variable (Generated mon_235)) Pure))
              Ctl))
            (pure_branch
             ((Variable (Generated mon_239)) (Variable (Generated mon_239))))))))
        (Let (Variable (Generated mon_242)) Pure
         (Application (Variable (Language perform))
          (((Effect_label console) Pure)
           ((Lambda
             ((((Variable (Generated mon_241)) Pure)) Pure
              (Select_operation console (User println)
               (Variable (Generated mon_241)))))
            Pure))
          Pure)
         (Let (Variable (Generated mon_243)) Pure (Tuple_construction ())
          (Application (Variable (Generated mon_242))
           (((Variable (Generated mon_243)) Pure)
            ((Variable (Generated mon_235)) Pure))
           Ctl))))))
     ((User head)
      ((((Variable (User xs)) Pure) ((Variable (Generated mon_244)) Pure)) Ctl
       (Let (Variable (Generated mon_245)) Pure (Variable (User xs))
        (Match (Variable (Generated mon_245)) List
         (((Construction List_nil ())
           (Construct_pure (Construction Option_none ())))
          ((Construction List_cons ((Variable (User x)) Wildcard))
           (Let (Variable (Generated mon_246)) Pure (Variable (User x))
            (Construct_pure
             (Construction Option_some ((Variable (Generated mon_246))))))))))))
     ((User some-if)
      ((((Variable (User cond)) Pure) ((Variable (User value)) Pure)
        ((Variable (Generated mon_247)) Pure))
       Ctl
       (Let (Variable (Generated mon_248)) Pure (Variable (User cond))
        (If_then_else (Variable (Generated mon_248))
         (Let (Variable (Generated mon_249)) Pure (Variable (User value))
          (Construct_pure
           (Construction Option_some ((Variable (Generated mon_249))))))
         (Construct_pure (Construction Option_none ()))))))
     ((Generated opt_60)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_254)) Pure)
          ((Variable (Generated mon_255)) Pure))
         Ctl
         (Construct_pure
          (Construction Option_some ((Variable (Generated mon_254)))))))))
     ((User option-map)
      ((((Variable (User x)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_250)) Pure))
       Ctl
       (Let (Variable (Generated mon_251)) Pure (Variable (User x))
        (Match (Variable (Generated mon_251)) Option
         (((Construction Option_none ())
           (Construct_pure (Construction Option_none ())))
          ((Construction Option_some ((Variable (User y))))
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_252)) Pure (Variable (User f))
              (Let (Variable (Generated mon_253)) Pure (Variable (User y))
               (Application (Variable (Generated mon_252))
                (((Variable (Generated mon_253)) Pure)
                 ((Variable (Generated mon_250)) Pure))
                Ctl))))
            (pure_branch
             ((Variable (Generated mon_254))
              (Let (Variable (Generated mon_255)) Pure
               (Variable (Generated mon_250))
               (Construct_pure
                (Construction Option_some ((Variable (Generated mon_254))))))))
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
                    ((Application (Variable (Generated opt_60)) () Pure) Pure))
                   Ctl))))))))))))))
     ((User option-flat-map)
      ((((Variable (User x)) Pure) ((Variable (User f)) Pure)
        ((Variable (Generated mon_256)) Pure))
       Ctl
       (Let (Variable (Generated mon_257)) Pure (Variable (User x))
        (Match (Variable (Generated mon_257)) Option
         (((Construction Option_none ())
           (Construct_pure (Construction Option_none ())))
          ((Construction Option_some ((Variable (User y))))
           (Let (Variable (Generated mon_258)) Pure (Variable (User f))
            (Let (Variable (Generated mon_259)) Pure (Variable (User y))
             (Application (Variable (Generated mon_258))
              (((Variable (Generated mon_259)) Pure)
               ((Variable (Generated mon_256)) Pure))
              Ctl)))))))))
     ((Generated opt_66)
      ((((Variable (User a2)) Pure) ((Variable (User b2)) Pure)
        ((Variable (User equal2)) Pure))
       Pure
       (Lambda
        ((((Variable (Generated mon_264)) Pure)
          ((Variable (Generated mon_265)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_264))
          (Let (Variable (Generated mon_266)) Pure (Variable (User equal2))
           (Let (Variable (Generated mon_267)) Pure (Variable (User a2))
            (Let (Variable (Generated mon_268)) Pure (Variable (User b2))
             (Application (Variable (Generated mon_266))
              (((Variable (Generated mon_267)) Pure)
               ((Variable (Generated mon_268)) Pure)
               ((Variable (Generated mon_265)) Pure))
              Ctl))))
          (Construct_pure (Literal (Bool false))))))))
     ((User tuple2-equal)
      ((((Tuple ((Variable (User a1)) (Variable (User a2)))) Pure)
        ((Tuple ((Variable (User b1)) (Variable (User b2)))) Pure)
        ((Variable (User equal1)) Pure) ((Variable (User equal2)) Pure)
        ((Variable (Generated mon_260)) Pure))
       Ctl
       (Match_ctl
        (subject
         (Let (Variable (Generated mon_261)) Pure (Variable (User equal1))
          (Let (Variable (Generated mon_262)) Pure (Variable (User a1))
           (Let (Variable (Generated mon_263)) Pure (Variable (User b1))
            (Application (Variable (Generated mon_261))
             (((Variable (Generated mon_262)) Pure)
              ((Variable (Generated mon_263)) Pure)
              ((Variable (Generated mon_260)) Pure))
             Ctl)))))
        (pure_branch
         ((Variable (Generated mon_264))
          (Let (Variable (Generated mon_265)) Pure
           (Variable (Generated mon_260))
           (If_then_else (Variable (Generated mon_264))
            (Let (Variable (Generated mon_266)) Pure (Variable (User equal2))
             (Let (Variable (Generated mon_267)) Pure (Variable (User a2))
              (Let (Variable (Generated mon_268)) Pure (Variable (User b2))
               (Application (Variable (Generated mon_266))
                (((Variable (Generated mon_267)) Pure)
                 ((Variable (Generated mon_268)) Pure)
                 ((Variable (Generated mon_265)) Pure))
                Ctl))))
            (Construct_pure (Literal (Bool false)))))))
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
                ((Application (Variable (Generated opt_66))
                  (((Variable (User a2)) Pure) ((Variable (User b2)) Pure)
                   ((Variable (User equal2)) Pure))
                  Pure)
                 Pure))
               Ctl))))))))))
     ((Generated opt_93)
      ((((Variable (Generated mon_270)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_273)) Pure)
          ((Variable (Generated mon_274)) Pure))
         Ctl
         (Application (Variable (Generated mon_270))
          (((Variable (Generated mon_273)) Pure)
           ((Variable (Generated mon_274)) Pure))
          Ctl)))))
     ((Generated opt_92)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_275)) Pure)
          ((Variable (Generated mon_276)) Pure))
         Ctl
         (Let (Variable (Generated mon_277)) Pure (Literal (Int 0))
          (Construct_pure
           (Operator (Variable (Generated mon_275)) (Int Greater_than)
            (Variable (Generated mon_277)))))))))
     ((Generated opt_79)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_282)) Pure)
          ((Variable (Generated mon_283)) Pure))
         Ctl
         (Let (Variable (Generated mon_284)) Pure (Literal (Int 1))
          (Application (Variable (Generated mon_282))
           (((Variable (Generated mon_284)) Pure)
            ((Variable (Generated mon_283)) Pure))
           Ctl))))))
     ((Generated opt_85)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_287)) Pure)
          ((Variable (Generated mon_288)) Pure))
         Ctl
         (Let (Variable (Generated mon_289)) Pure (Literal (Int 0))
          (Application (Variable (Generated mon_287))
           (((Variable (Generated mon_289)) Pure)
            ((Variable (Generated mon_288)) Pure))
           Ctl))))))
     ((Generated opt_91)
      ((((Variable (User h)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_278)) Pure)
          ((Variable (Generated mon_279)) Pure))
         Ctl
         (If_then_else (Variable (Generated mon_278))
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_280)) Pure (Variable (User h))
              (Let (Variable (Generated mon_281)) Pure (Literal (Int 0))
               (Application (Variable (Generated mon_280))
                (((Variable (Generated mon_281)) Pure)
                 ((Variable (Generated mon_279)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_279)) Pure)
            ((Application (Variable (Generated opt_79)) () Pure) Pure))
           Ctl)
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_285)) Pure (Variable (User h))
              (Let (Variable (Generated mon_286)) Pure (Literal (Int 1))
               (Application (Variable (Generated mon_285))
                (((Variable (Generated mon_286)) Pure)
                 ((Variable (Generated mon_279)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_279)) Pure)
            ((Application (Variable (Generated opt_85)) () Pure) Pure))
           Ctl))))))
     ((Generated opt_72)
      ((((Variable (User z)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_294)) Pure)
          ((Variable (Generated mon_295)) Pure))
         Ctl
         (Let (Variable (Generated mon_296)) Pure (Variable (User z))
          (Application (Variable (Generated mon_294))
           (((Variable (Generated mon_296)) Pure)
            ((Variable (Generated mon_295)) Pure))
           Ctl))))))
     ((Generated opt_78)
      ((((Variable (User h)) Pure)) Pure
       (Lambda
        ((((Variable (Generated mon_290)) Pure)
          ((Variable (Generated mon_291)) Pure))
         Ctl
         (Let (Variable (User z)) Pure (Variable (Generated mon_290))
          (Application (Variable (Language bind))
           (((Let (Variable (Generated mon_292)) Pure (Variable (User h))
              (Let (Variable (Generated mon_293)) Pure (Variable (User z))
               (Application (Variable (Generated mon_292))
                (((Variable (Generated mon_293)) Pure)
                 ((Variable (Generated mon_291)) Pure))
                Ctl)))
             Ctl)
            ((Variable (Generated mon_291)) Pure)
            ((Application (Variable (Generated opt_72))
              (((Variable (User z)) Pure)) Pure)
             Pure))
           Ctl))))))
     ((User foo)
      ((((Variable (User f)) Pure) ((Variable (User g)) Pure)
        ((Variable (User h)) Pure) ((Variable (User x)) Pure)
        ((Variable (Generated mon_269)) Pure))
       Ctl
       (Match_ctl
        (subject
         (Match_ctl
          (subject
           (Match_ctl
            (subject
             (Let (Variable (Generated mon_270)) Pure (Variable (User f))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_271)) Pure (Variable (User g))
                 (Let (Variable (Generated mon_272)) Pure (Variable (User x))
                  (Application (Variable (Generated mon_271))
                   (((Variable (Generated mon_272)) Pure)
                    ((Variable (Generated mon_269)) Pure))
                   Ctl))))
               (pure_branch
                ((Variable (Generated mon_273))
                 (Let (Variable (Generated mon_274)) Pure
                  (Variable (Generated mon_269))
                  (Application (Variable (Generated mon_270))
                   (((Variable (Generated mon_273)) Pure)
                    ((Variable (Generated mon_274)) Pure))
                   Ctl))))
               (yield_branch
                ((Generated opt_94) (Generated opt_95) (Generated opt_96)
                 (Construct_yield (marker (Variable (Generated opt_94)))
                  (op_clause (Variable (Generated opt_95)))
                  (resumption
                   (Lambda
                    ((((Variable (Generated opt_97)) Pure)
                      ((Variable (Generated opt_98)) Pure))
                     Ctl
                     (Application (Variable (Language bind))
                      (((Application (Variable (Generated opt_96))
                         (((Variable (Generated opt_97)) Pure)
                          ((Variable (Generated opt_98)) Pure))
                         Ctl)
                        Ctl)
                       ((Variable (Generated opt_98)) Pure)
                       ((Application (Variable (Generated opt_93))
                         (((Variable (Generated mon_270)) Pure)) Pure)
                        Pure))
                      Ctl))))))))))
            (pure_branch
             ((Variable (Generated mon_275))
              (Let (Variable (Generated mon_276)) Pure
               (Variable (Generated mon_269))
               (Let (Variable (Generated mon_277)) Pure (Literal (Int 0))
                (Construct_pure
                 (Operator (Variable (Generated mon_275)) (Int Greater_than)
                  (Variable (Generated mon_277))))))))
            (yield_branch
             ((Generated opt_99) (Generated opt_100) (Generated opt_101)
              (Construct_yield (marker (Variable (Generated opt_99)))
               (op_clause (Variable (Generated opt_100)))
               (resumption
                (Lambda
                 ((((Variable (Generated opt_102)) Pure)
                   ((Variable (Generated opt_103)) Pure))
                  Ctl
                  (Application (Variable (Language bind))
                   (((Application (Variable (Generated opt_101))
                      (((Variable (Generated opt_102)) Pure)
                       ((Variable (Generated opt_103)) Pure))
                      Ctl)
                     Ctl)
                    ((Variable (Generated opt_103)) Pure)
                    ((Application (Variable (Generated opt_92)) () Pure) Pure))
                   Ctl)))))))))
          (pure_branch
           ((Variable (Generated mon_278))
            (Let (Variable (Generated mon_279)) Pure
             (Variable (Generated mon_269))
             (If_then_else (Variable (Generated mon_278))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_280)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_281)) Pure (Literal (Int 0))
                  (Application (Variable (Generated mon_280))
                   (((Variable (Generated mon_281)) Pure)
                    ((Variable (Generated mon_279)) Pure))
                   Ctl))))
               (pure_branch
                ((Variable (Generated mon_282))
                 (Let (Variable (Generated mon_283)) Pure
                  (Variable (Generated mon_279))
                  (Let (Variable (Generated mon_284)) Pure (Literal (Int 1))
                   (Application (Variable (Generated mon_282))
                    (((Variable (Generated mon_284)) Pure)
                     ((Variable (Generated mon_283)) Pure))
                    Ctl)))))
               (yield_branch
                ((Generated opt_80) (Generated opt_81) (Generated opt_82)
                 (Construct_yield (marker (Variable (Generated opt_80)))
                  (op_clause (Variable (Generated opt_81)))
                  (resumption
                   (Lambda
                    ((((Variable (Generated opt_83)) Pure)
                      ((Variable (Generated opt_84)) Pure))
                     Ctl
                     (Application (Variable (Language bind))
                      (((Application (Variable (Generated opt_82))
                         (((Variable (Generated opt_83)) Pure)
                          ((Variable (Generated opt_84)) Pure))
                         Ctl)
                        Ctl)
                       ((Variable (Generated opt_84)) Pure)
                       ((Application (Variable (Generated opt_79)) () Pure)
                        Pure))
                      Ctl))))))))
              (Match_ctl
               (subject
                (Let (Variable (Generated mon_285)) Pure (Variable (User h))
                 (Let (Variable (Generated mon_286)) Pure (Literal (Int 1))
                  (Application (Variable (Generated mon_285))
                   (((Variable (Generated mon_286)) Pure)
                    ((Variable (Generated mon_279)) Pure))
                   Ctl))))
               (pure_branch
                ((Variable (Generated mon_287))
                 (Let (Variable (Generated mon_288)) Pure
                  (Variable (Generated mon_279))
                  (Let (Variable (Generated mon_289)) Pure (Literal (Int 0))
                   (Application (Variable (Generated mon_287))
                    (((Variable (Generated mon_289)) Pure)
                     ((Variable (Generated mon_288)) Pure))
                    Ctl)))))
               (yield_branch
                ((Generated opt_86) (Generated opt_87) (Generated opt_88)
                 (Construct_yield (marker (Variable (Generated opt_86)))
                  (op_clause (Variable (Generated opt_87)))
                  (resumption
                   (Lambda
                    ((((Variable (Generated opt_89)) Pure)
                      ((Variable (Generated opt_90)) Pure))
                     Ctl
                     (Application (Variable (Language bind))
                      (((Application (Variable (Generated opt_88))
                         (((Variable (Generated opt_89)) Pure)
                          ((Variable (Generated opt_90)) Pure))
                         Ctl)
                        Ctl)
                       ((Variable (Generated opt_90)) Pure)
                       ((Application (Variable (Generated opt_85)) () Pure)
                        Pure))
                      Ctl))))))))))))
          (yield_branch
           ((Generated opt_104) (Generated opt_105) (Generated opt_106)
            (Construct_yield (marker (Variable (Generated opt_104)))
             (op_clause (Variable (Generated opt_105)))
             (resumption
              (Lambda
               ((((Variable (Generated opt_107)) Pure)
                 ((Variable (Generated opt_108)) Pure))
                Ctl
                (Application (Variable (Language bind))
                 (((Application (Variable (Generated opt_106))
                    (((Variable (Generated opt_107)) Pure)
                     ((Variable (Generated opt_108)) Pure))
                    Ctl)
                   Ctl)
                  ((Variable (Generated opt_108)) Pure)
                  ((Application (Variable (Generated opt_91))
                    (((Variable (User h)) Pure)) Pure)
                   Pure))
                 Ctl)))))))))
        (pure_branch
         ((Variable (Generated mon_290))
          (Let (Variable (Generated mon_291)) Pure
           (Variable (Generated mon_269))
           (Let (Variable (User z)) Pure (Variable (Generated mon_290))
            (Match_ctl
             (subject
              (Let (Variable (Generated mon_292)) Pure (Variable (User h))
               (Let (Variable (Generated mon_293)) Pure (Variable (User z))
                (Application (Variable (Generated mon_292))
                 (((Variable (Generated mon_293)) Pure)
                  ((Variable (Generated mon_291)) Pure))
                 Ctl))))
             (pure_branch
              ((Variable (Generated mon_294))
               (Let (Variable (Generated mon_295)) Pure
                (Variable (Generated mon_291))
                (Let (Variable (Generated mon_296)) Pure (Variable (User z))
                 (Application (Variable (Generated mon_294))
                  (((Variable (Generated mon_296)) Pure)
                   ((Variable (Generated mon_295)) Pure))
                  Ctl)))))
             (yield_branch
              ((Generated opt_73) (Generated opt_74) (Generated opt_75)
               (Construct_yield (marker (Variable (Generated opt_73)))
                (op_clause (Variable (Generated opt_74)))
                (resumption
                 (Lambda
                  ((((Variable (Generated opt_76)) Pure)
                    ((Variable (Generated opt_77)) Pure))
                   Ctl
                   (Application (Variable (Language bind))
                    (((Application (Variable (Generated opt_75))
                       (((Variable (Generated opt_76)) Pure)
                        ((Variable (Generated opt_77)) Pure))
                       Ctl)
                      Ctl)
                     ((Variable (Generated opt_77)) Pure)
                     ((Application (Variable (Generated opt_72))
                       (((Variable (User z)) Pure)) Pure)
                      Pure))
                    Ctl))))))))))))
        (yield_branch
         ((Generated opt_109) (Generated opt_110) (Generated opt_111)
          (Construct_yield (marker (Variable (Generated opt_109)))
           (op_clause (Variable (Generated opt_110)))
           (resumption
            (Lambda
             ((((Variable (Generated opt_112)) Pure)
               ((Variable (Generated opt_113)) Pure))
              Ctl
              (Application (Variable (Language bind))
               (((Application (Variable (Generated opt_111))
                  (((Variable (Generated opt_112)) Pure)
                   ((Variable (Generated opt_113)) Pure))
                  Ctl)
                 Ctl)
                ((Variable (Generated opt_113)) Pure)
                ((Application (Variable (Generated opt_78))
                  (((Variable (User h)) Pure)) Pure)
                 Pure))
               Ctl))))))))))
     ((User main)
      ((((Variable (Generated mon_297)) Pure)) Ctl
       (Let (Variable (Generated mon_299)) Pure
        (Application (Variable (Language perform))
         (((Effect_label console) Pure)
          ((Lambda
            ((((Variable (Generated mon_298)) Pure)) Pure
             (Select_operation console (User println-int)
              (Variable (Generated mon_298)))))
           Pure))
         Pure)
        (Let (Variable (Generated mon_316)) Pure
         (Let (Variable (Generated mon_300)) Pure (Variable (User foo))
          (Let (Variable (Generated mon_304)) Pure
           (Lambda
            ((((Variable (User y)) Pure) ((Variable (Generated mon_301)) Pure))
             Ctl
             (Let (Variable (Generated mon_302)) Pure (Variable (User y))
              (Let (Variable (Generated mon_303)) Pure (Variable (User y))
               (Construct_pure
                (Operator (Variable (Generated mon_302)) (Int Times)
                 (Variable (Generated mon_303))))))))
           (Let (Variable (Generated mon_308)) Pure
            (Lambda
             ((((Variable (User x)) Pure)
               ((Variable (Generated mon_305)) Pure))
              Ctl
              (Let (Variable (Generated mon_306)) Pure (Variable (User x))
               (Let (Variable (Generated mon_307)) Pure (Variable (User x))
                (Construct_pure
                 (Operator (Variable (Generated mon_306)) (Int Plus)
                  (Variable (Generated mon_307))))))))
            (Let (Variable (Generated mon_313)) Pure
             (Lambda
              ((((Variable (User m)) Pure)
                ((Variable (Generated mon_309)) Pure))
               Ctl
               (Construct_pure
                (Lambda
                 ((((Variable (User n)) Pure)
                   ((Variable (Generated mon_310)) Pure))
                  Ctl
                  (Let (Variable (Generated mon_311)) Pure (Variable (User m))
                   (Let (Variable (Generated mon_312)) Pure (Variable (User n))
                    (Construct_pure
                     (Operator (Variable (Generated mon_311)) (Int Minus)
                      (Variable (Generated mon_312)))))))))))
             (Let (Variable (Generated mon_314)) Pure (Literal (Int 3))
              (Match_ctl_pure
               (subject
                (Application (Variable (Generated mon_300))
                 (((Variable (Generated mon_304)) Pure)
                  ((Variable (Generated mon_308)) Pure)
                  ((Variable (Generated mon_313)) Pure)
                  ((Variable (Generated mon_314)) Pure)
                  ((Variable (Generated mon_297)) Pure))
                 Ctl))
               (pure_branch
                ((Variable (Generated mon_315)) (Variable (Generated mon_315))))))))))
         (Application (Variable (Generated mon_299))
          (((Variable (Generated mon_316)) Pure)
           ((Variable (Generated mon_297)) Pure))
          Ctl)))))
     ((Generated opt_114)
      (() Pure
       (Lambda
        ((((Variable (Generated mon_326)) Pure)
          ((Variable (Generated mon_327)) Pure))
         Ctl (Construct_pure (Tuple_construction ()))))))
     ((Language main)
      ((((Variable (Generated mon_317)) Pure)) Ctl
       (Match_ctl
        (subject
         (Let (Variable (Generated mon_324)) Pure
          (Application (Variable (Language handler))
           (((Effect_label console) Pure)
            ((Construct_handler (handled_effect console)
              (operation_clauses
               (((User print-int)
                 (Construct_op_tail
                  (Lambda
                   ((((Variable (Language x)) Pure)
                     ((Variable (Generated mon_318)) Pure))
                    Ctl
                    (Let (Variable (Generated mon_319)) Pure
                     (Variable (Language x))
                     (Construct_pure
                      (Impure_built_in
                       (Impure_print_int (value (Variable (Generated mon_319)))
                        (newline false)))))))))
                ((User println)
                 (Construct_op_tail
                  (Lambda
                   (((Wildcard Pure) ((Variable (Generated mon_320)) Pure)) Ctl
                    (Construct_pure (Impure_built_in Impure_println))))))
                ((User println-int)
                 (Construct_op_tail
                  (Lambda
                   ((((Variable (Language x)) Pure)
                     ((Variable (Generated mon_321)) Pure))
                    Ctl
                    (Let (Variable (Generated mon_322)) Pure
                     (Variable (Language x))
                     (Construct_pure
                      (Impure_built_in
                       (Impure_print_int (value (Variable (Generated mon_322)))
                        (newline true)))))))))
                ((User read-int)
                 (Construct_op_tail
                  (Lambda
                   (((Wildcard Pure) ((Variable (Generated mon_323)) Pure)) Ctl
                    (Construct_pure (Impure_built_in Impure_read_int)))))))))
             Pure))
           Pure)
          (Let (Variable (Generated mon_325)) Pure (Variable (User main))
           (Application (Variable (Generated mon_324))
            (((Variable (Generated mon_325)) Pure)
             ((Variable (Generated mon_317)) Pure))
            Ctl))))
        (pure_branch
         ((Variable (Generated mon_326))
          (Let (Variable (Generated mon_327)) Pure
           (Variable (Generated mon_317))
           (Construct_pure (Tuple_construction ())))))
        (yield_branch
         ((Generated opt_115) (Generated opt_116) (Generated opt_117)
          (Construct_yield (marker (Variable (Generated opt_115)))
           (op_clause (Variable (Generated opt_116)))
           (resumption
            (Lambda
             ((((Variable (Generated opt_118)) Pure)
               ((Variable (Generated opt_119)) Pure))
              Ctl
              (Application (Variable (Language bind))
               (((Application (Variable (Generated opt_117))
                  (((Variable (Generated opt_118)) Pure)
                   ((Variable (Generated opt_119)) Pure))
                  Ctl)
                 Ctl)
                ((Variable (Generated opt_119)) Pure)
                ((Application (Variable (Generated opt_114)) () Pure) Pure))
               Ctl))))))))))))
   (entry_expr
    (Application (Variable (Language main)) ((Nil_evidence_vector Pure)) Ctl)))
