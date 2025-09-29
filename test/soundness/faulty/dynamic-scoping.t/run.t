Attempting to access variable bound in call scope, rather than defintion scope
  $ export PROJECT_ROOT=../../../..
  $ ../../../koka-zero.sh check dynamic-scoping.kk
  type error: unbound variable: y
  [1]
