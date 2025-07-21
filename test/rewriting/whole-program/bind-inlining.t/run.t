Show the generated code for a program with many binds
  $ export PROJECT_ROOT=../../../..
  $ export DUMP_EPS=1

First show without any rewriting optimisations
  $ ../compile.sh many-binds.kk -dump-eps
  (Sys_error "../../../../koka-zero-config.sexp: No such file or directory")
  [1]

Then show with bind-inlining and other rewriting applied
  $ OPT_LEVEL=2 ../compile.sh many-binds.kk -dump-eps
  (Sys_error "../../../../koka-zero-config.sexp: No such file or directory")
  [1]
