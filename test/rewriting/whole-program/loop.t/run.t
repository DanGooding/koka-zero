Show the generated code for a program with many binds
  $ export PROJECT_ROOT=../../../..

First show without any rewriting optimisations
  $ ../../koka-zero.sh compile loop.kk -dump-eps
  ../../koka-zero.sh: No such file or directory
  [127]

Then show with bind-inlining and other rewriting applied
  $ ../../koka-zero.sh compile loop.kk -dump-eps -optimise
  ../../koka-zero.sh: No such file or directory
  [127]
