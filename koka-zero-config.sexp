;; This config specifies the dependencies required for compilation
;; it is checked in for convenience only.
;; We should probably explicitly depend on them via package-manager deps,
;; or find them once at install-time and put this config in the user's homedir.
((clang_exe /usr/bin/clang)            
 (runtime_path /Users/dan/projects/koka-zero/lib/execution/runtime/runtime.c)
 (gc_path (/opt/homebrew/Cellar/bdw-gc/8.2.8)))