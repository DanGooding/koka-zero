A handler which doesn't structurally match any effect
  $ export PROJECT_ROOT=../../../..
  $ ../../../koka-zero.sh check handler-missing-operation.kk
  type error: handler does not match any effect: ((User get))
  [1]
