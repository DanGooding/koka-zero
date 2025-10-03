Implement an optimisation search using effects
  $ export PROJECT_ROOT=../../..

  $ ../../koka-zero.sh interpret optimisation.kk
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

