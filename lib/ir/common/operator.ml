open! Core
open! Import

module Int = struct
  type t =
    | Plus
    | Minus
    | Times
    | Divide
    | Modulo
    | Equals
    | Not_equal
    | Less_than
    | Less_equal
    | Greater_than
    | Greater_equal
  [@@deriving sexp_of]
end

module Bool = struct
  module Unary = struct
    type t = Not [@@deriving sexp_of]
  end

  type t =
    | And
    | Or
  [@@deriving sexp_of]
end

module Unary = struct
  type t = Bool of Bool.Unary.t [@@deriving sexp_of]
end

type t =
  | Int of Int.t
  | Bool of Bool.t
[@@deriving sexp_of]
