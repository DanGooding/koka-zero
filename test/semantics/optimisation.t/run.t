Implement an optimisation search using effects
  $ export PROJECT_ROOT=../../..

  $ ../../koka-zero.sh interpret optimisation.kk
  runtime error: impossible error: ("expression expected to be pure returned Yield"
   (expr
    (Match_ctl_pure
     (subject
      (Application (Variable (Generated mon_84))
       (((Variable (Generated mon_85)) Pure)
        ((Variable (Generated mon_91)) Pure)
        ((Variable (Generated mon_93)) Pure)
        ((Variable (Generated mon_92)) Pure))
       Ctl))
     (pure_branch
      ((Variable (Generated mon_94)) (Variable (Generated mon_94)))))))
  
  [2]

  $ ../../koka-zero.sh compile optimisation.kk
  $ ./optimisation
  1 1 1 
  1 1 2 
  1 1 3 
  1 2 1 
  1 2 2 
  1 2 3 
  1 3 1 
  1 3 2 
  1 3 3 
  2 1 1 
  2 1 2 
  2 1 3 
  2 2 1 
  2 2 2 
  2 2 3 
  2 3 1 
  2 3 2 
  2 3 3 
  3 1 1 
  3 1 2 
  3 1 3 
  3 2 1 
  3 2 2 
  3 2 3 
  3 3 1 
  3 3 2 
  3 3 3 

