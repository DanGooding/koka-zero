open! Core
open! Import

module Evidence_entry : sig
  module Field : sig
    type t =
      | Handler
      | Marker
      | Handler_site_vector
  end

  include Struct.Struct_S with module Field := Field and type t = unit
end

module Ctl_yield : sig
  module Field : sig
    type t =
      | Marker
      | Op_clause
      | Resumption
  end

  include Struct.Struct_S with module Field := Field and type t = unit
end

module Closure : sig
  type t = { num_captured : int }

  module Field : sig
    type t =
      | Code_address
      | Capture of { index : int }
  end

  include Struct.Struct_S with module Field := Field and type t := t
end

module Handler : sig
  type t = { num_ops : int }

  module Field : sig
    type t = Op of { index : int }
  end

  include Struct.Struct_S with module Field := Field and type t := t
end

module Op : sig
  module Tag : sig
    val const_normal : Llvm.llvalue Codegen.t
    val const_tail : Llvm.llvalue Codegen.t
  end

  module Field : sig
    type t =
      | Tag
      | Clause
  end

  include Struct.Struct_S with module Field := Field and type t = unit
end

module List : sig
  module Tag : sig
    val const_nil : Llvm.llvalue Codegen.t
    val const_cons : Llvm.llvalue Codegen.t
  end

  module Field : sig
    type t =
      | Tag
      | Head
      | Tail
  end

  include Struct.Struct_S with module Field := Field and type t = unit
end

module Option : sig
  module Tag : sig
    val const_none : Llvm.llvalue Codegen.t
    val const_some : Llvm.llvalue Codegen.t
  end

  module Field : sig
    type t =
      | Tag
      | Value
  end

  include Struct.Struct_S with module Field := Field and type t = unit
end

module Tuple : sig
  type t = { num_elements : int }

  module Field : sig
    type t = Element of { index : int }
  end

  include Struct.Struct_S with module Field := Field and type t := t
end
