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

  (* build each branch, but don't add terminators (branch or return) to each block yet *)
  let fanout ~cond_i1 ~compile_true ~compile_false =
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
    let%bind true_branch_result = compile_true () in
    let%bind true_end_block = Codegen.insertion_block_exn in
    (* compile false branch *)
    let%bind () =
      Codegen.use_builder (Llvm.position_at_end false_start_block)
    in
    let%bind false_branch_result = compile_false () in
    let%map false_end_block = Codegen.insertion_block_exn in
    [ true_branch_result, true_end_block; false_branch_result, false_end_block ]
  ;;

  let recombining ~phi_builder ~cond_i1 ~compile_true ~compile_false =
    let open Codegen.Let_syntax in
    let%bind current_block = Codegen.insertion_block_exn in
    let current_function = Llvm.block_parent current_block in
    let%bind if_end_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "post_if" current_function)
    in
    let recombine_after compile_branch () =
      let%bind result = compile_branch () in
      let%map _br_end = Codegen.use_builder (Llvm.build_br if_end_block) in
      result
    in
    let%bind results_and_end_blocks =
      fanout
        ~cond_i1
        ~compile_true:(recombine_after compile_true)
        ~compile_false:(recombine_after compile_false)
    in
    (* connect back together *)
    let%bind () = Codegen.use_builder (Llvm.position_at_end if_end_block) in
    phi_builder results_and_end_blocks
  ;;

  (* a tail-position conditional has a [return] at the end of each branch,
     so we don't need to recombine the branches *)
  let tail_position ~cond_i1 ~compile_true ~compile_false =
    let open Codegen.Let_syntax in
    let%map (_results_and_end_blocks : (unit * Llvm.llbasicblock) list) =
      fanout ~cond_i1 ~compile_true ~compile_false
    in
    ()
  ;;
end

module Compile_switch = struct
  type 'a t =
    Llvm.llvalue
    -> table:(Llvm.llvalue * string * (unit -> 'a Codegen.t)) list
    -> compile_default:(unit -> [ `Result of 'a | `Exits ] Codegen.t)
    -> 'a Codegen.t

  let fanout subject ~table ~compile_default =
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
    let%bind default_result = compile_default () in
    let%bind default_end_block = Codegen.insertion_block_exn in
    (* compile each branch *)
    let%map table_ends =
      List.map table ~f:(fun (tag, start_block, compile_branch) ->
        Llvm.add_case switch tag start_block;
        let%bind () = Codegen.use_builder (Llvm.position_at_end start_block) in
        let%bind branch_result = compile_branch () in
        let%map branch_end_block = Codegen.insertion_block_exn in
        branch_result, branch_end_block)
      |> Codegen.all
    in
    match default_result with
    | `Exits ->
      (* if the default branch exits, don't try to recombine it with the others *)
      table_ends
    | `Result default_result ->
      (default_result, default_end_block) :: table_ends
  ;;

  let recombining ~phi_builder subject ~table ~compile_default =
    let open Codegen.Let_syntax in
    let%bind current_block = Codegen.insertion_block_exn in
    let current_function = Llvm.block_parent current_block in
    let%bind post_switch_block =
      Codegen.use_context (fun context ->
        Llvm.append_block context "post_switch" current_function)
    in
    let recombine_after compile_branch () =
      let%bind result = compile_branch () in
      let%map _br = Codegen.use_builder (Llvm.build_br post_switch_block) in
      result
    in
    let%bind results_and_end_blocks =
      fanout
        subject
        ~table:
          (List.map table ~f:(fun (const, name, compile_branch) ->
             const, name, recombine_after compile_branch))
        ~compile_default:(fun () ->
          match%bind compile_default () with
          | `Exits -> return `Exits
          | `Result result ->
            let%map _br =
              Codegen.use_builder (Llvm.build_br post_switch_block)
            in
            `Result result)
    in
    (* connect back together *)
    let%bind () =
      Codegen.use_builder (Llvm.position_at_end post_switch_block)
    in
    phi_builder results_and_end_blocks
  ;;

  let tail_position =
    fun subject ~table ~compile_default ->
    let open Codegen.Let_syntax in
    let%map (_results_and_end_blocks : (unit * Llvm.llbasicblock) list) =
      fanout subject ~table ~compile_default
    in
    ()
  ;;
end
