open! Core
open! Import

module Int : sig
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

module Bool : sig
  module Unary : sig
    type t = Not [@@deriving sexp_of]
  end

  type t =
    | And
    | Or
  [@@deriving sexp_of]
end

module Unary : sig
  type t = Bool of Bool.Unary.t [@@deriving sexp_of]
end

type t =
  | Int of Int.t
  | Bool of Bool.t
[@@deriving sexp_of]
