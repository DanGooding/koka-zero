open Core
open Monad_utils_intf

module type S = S

(** usage (unfortunately rather verbose):

    {[
      module Thing = struct
        module T = struct
          ...
        end
        module T' = struct
          include T
          include Monad.Make (T)
        end

        include T'
        include Monad_utils.Make (T')
    ]} *)
module Make (M : Monad.S) : S with type 'a t = 'a M.t
