open! Core
open! Import

module Phi_builder = struct
  type 'a t = ('a * Llvm.llbasicblock) list -> 'a Codegen.t

  let llvalue incoming =
    Codegen.use_builder (Llvm.build_phi incoming "incoming")
  ;;
end

module Compile_conditional = struct
  type 'a t =
    cond_i1:Llvm.llvalue
    -> compile_true:(unit -> 'a Codegen.t)
    -> compile_false:(unit -> 'a Codegen.t)
    -> 'a Codegen.t

  let recombining ~phi_builder ~cond_i1 ~compile_true ~compile_false =
    let open Codegen.Let_syntax in
    let%bind if_start_block = Codegen.insertion_block_exn in
    let current_function = Llvm.block_parent if_start_block in
    let%bind true_start_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "if_true" current_function)
    in
    let%bind false_start_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "if_false" current_function)
    in
    let%bind if_end_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "post_if" current_function)
    in
    let%bind _branch =
      Codegen.use_builder
        (Llvm.build_cond_br cond_i1 true_start_block false_start_block)
    in
    (* compile yes branch *)
    let%bind () = Codegen.use_builder (Llvm.position_at_end true_start_block) in
    let%bind true_branch_result = compile_true () in
    let%bind _br_end = Codegen.use_builder (Llvm.build_br if_end_block) in
    let%bind true_end_block = Codegen.insertion_block_exn in
    (* compile false branch *)
    let%bind () =
      Codegen.use_builder (Llvm.position_at_end false_start_block)
    in
    let%bind false_branch_result = compile_false () in
    let%bind _br_end = Codegen.use_builder (Llvm.build_br if_end_block) in
    let%bind false_end_block = Codegen.insertion_block_exn in
    (* connect back together *)
    let%bind () = Codegen.use_builder (Llvm.position_at_end if_end_block) in
    phi_builder
      [ true_branch_result, true_end_block
      ; false_branch_result, false_end_block
      ]
  ;;

  (* a tail-position conditional has a [return] at the end of each branch,
     so we don't need to recombine the branches *)
  let tail_position ~cond_i1 ~compile_true ~compile_false =
    let open Codegen.Let_syntax in
    let%bind if_start_block = Codegen.insertion_block_exn in
    let current_function = Llvm.block_parent if_start_block in
    let%bind true_start_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "if_true" current_function)
    in
    let%bind false_start_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "if_false" current_function)
    in
    let%bind _branch =
      Codegen.use_builder
        (Llvm.build_cond_br cond_i1 true_start_block false_start_block)
    in
    (* compile yes branch *)
    let%bind () = Codegen.use_builder (Llvm.position_at_end true_start_block) in
    let%bind () = compile_true () in
    (* compile false branch *)
    let%bind () =
      Codegen.use_builder (Llvm.position_at_end false_start_block)
    in
    let%bind () = compile_false () in
    return ()
  ;;
end

module Compile_switch = struct
  type 'a t =
    Llvm.llvalue
    -> table:(Llvm.llvalue * string * (unit -> 'a Codegen.t)) list
    -> compile_default:(unit -> 'a Codegen.t)
    -> 'a Codegen.t

  let recombining =
    fun ~phi_builder subject ~table ~compile_default ->
    let open Codegen.Let_syntax in
    let%bind switch_start_block = Codegen.insertion_block_exn in
    let current_function = Llvm.block_parent switch_start_block in
    (* create start BB for each branch + default *)
    let%bind default_start_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "switch_default" current_function)
    in
    let%bind table =
      List.map table ~f:(fun (tag, name, compile_branch) ->
        Codegen.use_context (fun context ->
          let start_block =
            Llvm.append_block context ("switch_" ^ name) current_function
          in
          tag, start_block, compile_branch))
      |> Codegen.all
    in
    (* create post switch BB *)
    let%bind post_switch_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "post_switch" current_function)
    in
    (* create switch *)
    let%bind switch =
      Codegen.use_builder
        (Llvm.build_switch subject default_start_block (List.length table))
    in
    (* compile default branch *)
    let%bind () =
      Codegen.use_builder (Llvm.position_at_end default_start_block)
    in
    let%bind default_result = compile_default () in
    let%bind _br_post = Codegen.use_builder (Llvm.build_br post_switch_block) in
    let%bind default_end_block = Codegen.insertion_block_exn in
    (* compile each branch *)
    let%bind table_ends =
      List.map table ~f:(fun (tag, start_block, compile_branch) ->
        Llvm.add_case switch tag start_block;
        let%bind () = Codegen.use_builder (Llvm.position_at_end start_block) in
        let%bind branch_result = compile_branch () in
        let%bind _br_post =
          Codegen.use_builder (Llvm.build_br post_switch_block)
        in
        let%map branch_end_block = Codegen.insertion_block_exn in
        branch_result, branch_end_block)
      |> Codegen.all
    in
    (* create phi in post-switch *)
    let%bind () =
      Codegen.use_builder (Llvm.position_at_end post_switch_block)
    in
    let incoming = (default_result, default_end_block) :: table_ends in
    phi_builder incoming
  ;;

  let tail_position =
    fun subject ~table ~compile_default ->
    let open Codegen.Let_syntax in
    let%bind switch_start_block = Codegen.insertion_block_exn in
    let current_function = Llvm.block_parent switch_start_block in
    (* create start BB for each branch + default *)
    let%bind default_start_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "switch_default" current_function)
    in
    let%bind table =
      List.map table ~f:(fun (tag, name, compile_branch) ->
        Codegen.use_context (fun context ->
          let start_block =
            Llvm.append_block context ("switch_" ^ name) current_function
          in
          tag, start_block, compile_branch))
      |> Codegen.all
    in
    (* create switch *)
    let%bind switch =
      Codegen.use_builder
        (Llvm.build_switch subject default_start_block (List.length table))
    in
    (* compile default branch *)
    let%bind () =
      Codegen.use_builder (Llvm.position_at_end default_start_block)
    in
    let%bind () = compile_default () in
    (* compile each branch *)
    List.map table ~f:(fun (tag, start_block, compile_branch) ->
      Llvm.add_case switch tag start_block;
      let%bind () = Codegen.use_builder (Llvm.position_at_end start_block) in
      compile_branch ())
    |> Codegen.all_unit
  ;;
end
