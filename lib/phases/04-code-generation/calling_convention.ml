open! Core
open! Import
module Type = Evidence_passing_syntax.Type

(** a function [f] with args [a : Pure] [b : Ctl] and return [Ctl] will have
    the type:

    {v
    ptr f(
      ptr f_self,
      ptr closure, 
      ptr a, 
      ptr b_content,
      i1 b_is_yield,
      ptr return_is_yield)
    v} *)

module Function_type = struct
  type ('a, 'c, 'r) t =
    { function_repr : 'a
    ; closure : 'a
    ; args : [ `Pure of 'a | `Ctl of 'c ] list
    ; return_ : [ `Pure | `Ctl of 'r ]
    }

  let flatten_to_args
        { function_repr; closure; args; return_ }
        ~get_ctl_content
        ~get_ctl_is_yield_i1
    =
    let open Codegen.Let_syntax in
    let%bind pointer_type = Types.pointer in
    let function_repr = pointer_type, function_repr in
    let closure = pointer_type, closure in
    let%bind args =
      let%bind i1_type = Codegen.use_context Llvm.i1_type in
      Codegen.list_concat_map args ~f:(fun arg ->
        match arg with
        | `Pure value -> return [ pointer_type, value ]
        | `Ctl maybe_yield ->
          let%bind content = get_ctl_content maybe_yield in
          let%map is_yield_i1 = get_ctl_is_yield_i1 maybe_yield in
          [ pointer_type, content; i1_type, is_yield_i1 ])
    in
    let%map return_ =
      match return_ with
      | `Pure -> return None
      | `Ctl is_yield_i1_ptr ->
        let%map pointer_type = Types.pointer in
        Some (pointer_type, is_yield_i1_ptr)
    in
    (function_repr :: closure :: args) @ Option.to_list return_
  ;;

  let args_for_call
        (t : (Llvm.llvalue, Ctl_repr.Maybe_yield_repr.t, Llvm.llvalue) t)
    : (Llvm.lltype * Llvm.llvalue) list Codegen.t
    =
    flatten_to_args
      t
      ~get_ctl_content:Ctl_repr.Maybe_yield_repr.get_content
      ~get_ctl_is_yield_i1:Ctl_repr.Maybe_yield_repr.get_is_yield_i1
  ;;

  let params_for_definition (t : (string, string, string) t)
    : (Llvm.lltype * string) list Codegen.t
    =
    flatten_to_args
      t
      ~get_ctl_content:(fun ctl_name -> Codegen.return (ctl_name ^ "_content"))
      ~get_ctl_is_yield_i1:(fun ctl_name ->
        Codegen.return (ctl_name ^ "_is_yield_i1"))
  ;;

  let rec reconstruct (params : Llvm.llvalue list) (types : Type.t list)
    : Ctl_repr.t list
    =
    match params, types with
    | [], [] -> []
    | p :: params, Pure :: types -> Ctl_repr.Pure p :: reconstruct params types
    | content :: is_yield_i1 :: params, Ctl :: types ->
      Ctl (Ctl_repr.Maybe_yield_repr.create ~content ~is_yield_i1)
      :: reconstruct params types
    | [], (Pure | Ctl) :: _ | [ _ ], Ctl :: _ | _ :: _, [] ->
      raise_s
        [%message "failed to reconstruct params - types and params don't match"]
  ;;

  let map_unit (t : ('a, 'c, 'r) t) : (unit, unit, unit) t =
    { function_repr = ()
    ; closure = ()
    ; args =
        List.map t.args ~f:(function
          | `Pure _ -> `Pure ()
          | `Ctl _ -> `Ctl ())
    ; return_ =
        (match t.return_ with
         | `Pure -> `Pure
         | `Ctl _ -> `Ctl ())
    }
  ;;

  let attach_params (t : (unit, unit, unit) t) (params : Llvm.llvalue list)
    : (Llvm.llvalue, Ctl_repr.Maybe_yield_repr.t, Llvm.llvalue) t
    =
    match params with
    | function_repr :: closure :: params ->
      let params, return_ =
        match t.return_ with
        | `Pure ->
          let return_ = `Pure in
          params, return_
        | `Ctl () ->
          let is_yield_i1_pointer = List.last_exn params in
          let return_value_pointer = `Ctl is_yield_i1_pointer in
          let params = List.drop_last_exn params in
          params, return_value_pointer
      in
      let args =
        let param_types =
          List.map t.args ~f:(function
            | `Pure () -> Type.Pure
            | `Ctl () -> Ctl)
        in
        reconstruct params param_types
      in
      let args =
        List.map args ~f:(function
          | Ctl_repr.Pure arg -> `Pure arg
          | Ctl arg -> `Ctl arg)
      in
      { function_repr; closure; args; return_ }
    | _ -> raise_s [%message "function has wrong number of params"]
  ;;
end

let return_lltype (type_ : Type.t) =
  match type_ with
  | Pure | Ctl -> Types.pointer
;;

let compile_call
      ~(code_pointer : Llvm.llvalue)
      ~(function_repr : Function_repr.t)
      ~(closure : Llvm.llvalue)
      ~(args : Ctl_repr.t list)
      ~(return_type : Type.t)
  : Ctl_repr.t Codegen.t
  =
  let open Codegen.Let_syntax in
  let%bind f_self_arg = Function_repr.compile_wrap function_repr in
  let%bind return_arg =
    match (return_type : Type.t) with
    | Pure -> return `Pure
    | Ctl ->
      let%bind i1_type = Codegen.use_context Llvm.i1_type in
      let%bind return_is_yield_i1_pointer =
        Codegen.use_builder (Llvm.build_alloca i1_type "return_is_yield")
      in
      let zero = Llvm.const_null i1_type in
      let%map _store =
        Codegen.use_builder (Llvm.build_store zero return_is_yield_i1_pointer)
      in
      `Ctl return_is_yield_i1_pointer
  in
  let function_type =
    { Function_type.function_repr = f_self_arg
    ; closure
    ; args =
        List.map args ~f:(function
          | Ctl_repr.Pure arg -> `Pure arg
          | Ctl_repr.Ctl maybe_yield -> `Ctl maybe_yield)
    ; return_ = return_arg
    }
  in
  let%bind arg_types_and_values = Function_type.args_for_call function_type in
  let arg_types, arg_values = List.unzip arg_types_and_values in
  let%bind function_type =
    let%map return_type = return_lltype return_type in
    Llvm.function_type return_type (Array.of_list arg_types)
  in
  let%bind return_value =
    Codegen.use_builder
      (Llvm.build_call
         function_type
         code_pointer
         (Array.of_list arg_values)
         "call")
  in
  match return_arg with
  | `Pure -> return (Ctl_repr.Pure return_value)
  | `Ctl is_yield_i1_pointer ->
    let%bind i1_type = Codegen.use_context Llvm.i1_type in
    let%map is_yield_i1 =
      Codegen.use_builder
        (Llvm.build_load i1_type is_yield_i1_pointer "is_yield")
    in
    Ctl_repr.Ctl
      (Ctl_repr.Maybe_yield_repr.create ~content:return_value ~is_yield_i1)
;;

let make_function_and_context
      ~(params : (Parameter.t * Type.t) list)
      ~(symbol_name : Symbol_name.t)
      ~(self : Variable.t option)
      ~(return_type : Type.t)
      ~(captured_shape : Context.Closure.Shape.t)
      ~toplevel
  =
  let open Codegen.Let_syntax in
  let function_type_with_names =
    { Function_type.function_repr =
        Option.value_map
          self
          ~f:Helpers.register_name_of_variable
          ~default:"ignored"
    ; closure = "closure"
    ; args =
        List.map params ~f:(fun (name, type_) ->
          let root_name =
            match (name : Parameter.t) with
            | Variable name -> Helpers.register_name_of_variable name
            | Wildcard -> ""
          in
          match (type_ : Type.t) with
          | Pure -> `Pure root_name
          | Ctl -> `Ctl root_name)
    ; return_ =
        (match (return_type : Type.t) with
         | Pure -> `Pure
         | Ctl -> `Ctl "return")
    }
  in
  let%bind all_params_and_types =
    Function_type.params_for_definition function_type_with_names
  in
  let param_types, param_names = List.unzip all_params_and_types in
  let%bind function_type =
    let%map return_type = return_lltype return_type in
    Llvm.function_type return_type (Array.of_list param_types)
  in
  let%map function_ =
    Codegen.use_module
      (Llvm.define_function (Symbol_name.to_string symbol_name) function_type)
  in
  let param_llvalues = Llvm.params function_ |> Array.to_list in
  let () =
    List.zip_exn param_llvalues param_names
    |> List.iter ~f:(fun (param, name) -> Llvm.set_value_name name param)
  in
  let function_type_with_llvalues =
    Function_type.attach_params
      (Function_type.map_unit function_type_with_names)
      param_llvalues
  in
  let closure =
    { Context.Closure.closure = function_type_with_llvalues.closure
    ; shape = captured_shape
    }
  in
  let locals =
    let f_self =
      Option.map self ~f:(fun name ->
        name, Ctl_repr.Pure function_type_with_llvalues.function_repr)
    in
    let args =
      List.map function_type_with_llvalues.args ~f:(function
        | `Pure value -> Ctl_repr.Pure value
        | `Ctl maybe_yield -> Ctl_repr.Ctl maybe_yield)
    in
    let args =
      List.zip_exn params args
      |> List.filter_map ~f:(fun ((param, _type), arg) ->
        match param with
        | Variable name -> Some (name, arg)
        | Wildcard -> None)
    in
    Option.to_list f_self @ args
  in
  let return_value_pointer =
    match function_type_with_llvalues.return_ with
    | `Pure -> Context.Return_value_pointer.Pure
    | `Ctl is_yield_i1_pointer -> Ctl { is_yield_i1_pointer }
  in
  let context = { Context.toplevel; closure; locals; return_value_pointer } in
  function_, context
;;
