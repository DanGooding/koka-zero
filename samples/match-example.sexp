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
   ((User print-bool)
    ((((Variable (User b)) Pure) ((Variable (Generated mon_45)) Pure)) Ctl
     (Let (Variable (Generated mon_47)) Pure
      (Let (Variable (Generated mon_46)) Pure (Variable (User b))
       (Match (Variable (Generated mon_46)) (Primitive Bool)
        (((Literal (Bool false)) (Literal (Int 0)))
         ((Literal (Bool true)) (Literal (Int 1))))))
      (Let (Variable (User n)) Pure (Variable (Generated mon_47))
       (Let (Variable (Generated mon_49)) Pure
        (Application (Variable (Language perform))
         (((Effect_label console) Pure)
          ((Lambda
            ((((Variable (Generated mon_48)) Pure)) Pure
             (Select_operation console (User print-int)
              (Variable (Generated mon_48)))))
           Pure))
         Pure)
        (Let (Variable (Generated mon_50)) Pure (Variable (User n))
         (Application (Variable (Generated mon_49))
          (((Variable (Generated mon_50)) Pure)
           ((Variable (Generated mon_45)) Pure))
          Ctl)))))))
   ((User main)
    ((((Variable (Generated mon_51)) Pure)) Ctl
     (Application (Variable (Language bind))
      (((Let (Variable (Generated mon_52)) Pure (Variable (User print-bool))
         (Let (Variable (Generated mon_53)) Pure (Literal (Bool true))
          (Application (Variable (Generated mon_52))
           (((Variable (Generated mon_53)) Pure)
            ((Variable (Generated mon_51)) Pure))
           Ctl)))
        Ctl)
       ((Variable (Generated mon_51)) Pure)
       ((Lambda
         ((((Variable (Generated mon_54)) Pure)
           ((Variable (Generated mon_55)) Pure))
          Ctl
          (Let (Variable (Generated mon_57)) Pure
           (Application (Variable (Language perform))
            (((Effect_label console) Pure)
             ((Lambda
               ((((Variable (Generated mon_56)) Pure)) Pure
                (Select_operation console (User println)
                 (Variable (Generated mon_56)))))
              Pure))
            Pure)
           (Let (Variable (Generated mon_58)) Pure (Literal Unit)
            (Application (Variable (Generated mon_57))
             (((Variable (Generated mon_58)) Pure)
              ((Variable (Generated mon_55)) Pure))
             Ctl)))))
        Pure))
      Ctl)))
   ((Language main)
    ((((Variable (Generated mon_59)) Pure)) Ctl
     (Application (Variable (Language bind))
      (((Let (Variable (Generated mon_66)) Pure
         (Application (Variable (Language handler))
          (((Effect_label console) Pure)
           ((Construct_handler (handled_effect console)
             (operation_clauses
              (((User print-int)
                (Construct_op_tail
                 (Lambda
                  ((((Variable (Language x)) Pure)
                    ((Variable (Generated mon_60)) Pure))
                   Ctl
                   (Construct_pure
                    (Let (Variable (Generated mon_61)) Pure
                     (Variable (Language x))
                     (Impure_built_in
                      (Impure_print_int (value (Variable (Generated mon_61)))
                       (newline false)))))))))
               ((User println)
                (Construct_op_tail
                 (Lambda
                  (((Wildcard Pure) ((Variable (Generated mon_62)) Pure)) Ctl
                   (Construct_pure (Impure_built_in Impure_println))))))
               ((User println-int)
                (Construct_op_tail
                 (Lambda
                  ((((Variable (Language x)) Pure)
                    ((Variable (Generated mon_63)) Pure))
                   Ctl
                   (Construct_pure
                    (Let (Variable (Generated mon_64)) Pure
                     (Variable (Language x))
                     (Impure_built_in
                      (Impure_print_int (value (Variable (Generated mon_64)))
                       (newline true)))))))))
               ((User read-int)
                (Construct_op_tail
                 (Lambda
                  (((Wildcard Pure) ((Variable (Generated mon_65)) Pure)) Ctl
                   (Construct_pure (Impure_built_in Impure_read_int)))))))))
            Pure))
          Pure)
         (Let (Variable (Generated mon_67)) Pure (Variable (User main))
          (Application (Variable (Generated mon_66))
           (((Variable (Generated mon_67)) Pure)
            ((Variable (Generated mon_59)) Pure))
           Ctl)))
        Ctl)
       ((Variable (Generated mon_59)) Pure)
       ((Lambda
         ((((Variable (Generated mon_68)) Pure)
           ((Variable (Generated mon_69)) Pure))
          Ctl (Construct_pure (Literal Unit))))
        Pure))
      Ctl)))))
 (entry_expr
  (Application (Variable (Language main)) ((Nil_evidence_vector Pure)) Ctl)))
(compile_match (table ((_ false _) (_ true _))))
