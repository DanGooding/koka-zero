Fixing the handler-site vector `w_handler` in `under` neglects
the possiblity of the handler itself being suspended and resumed
somewhere else.

Taken from Section 2.9.2 of "Generalized Evidence Passing for Effect Handlers
(or, Efficient Compilation of Effect Handlers to C)"
by Ningning Xie and Daan Leijen

  $ koka-zero interpret underk.kk
  1
  2

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh underk.kk
  $ ./underk
  1
  2
