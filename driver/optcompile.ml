(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The batch compiler *)

open Misc
open Compile_common
open Typedtree

module Lambda_utils = struct
  open Lambda

let fold_lambda lvar llet =
  let rec aux expr =
    let insnd lst = List.map (fun (e,x) -> e, aux x) lst in
    let inopt = function
    | None -> None
    | Some x -> Some (aux x) in
  match expr with
  | Lvar x -> lvar x
  | Lconst _ -> expr
  | Llet (k,e,ident,l,r) ->
     llet aux k e ident l r
  | Lapply x ->
     let ap_func = aux x.ap_func in
     let ap_args = List.map aux x.ap_args in
     Lapply {x with ap_func; ap_args }
  | Lfunction x ->
     let body = aux x.body in
     Lfunction {x with body}
  | Lletrec (lst,l) ->
     Lletrec (insnd lst, aux l)
  | Lprim (a,lst,b) ->
     Lprim (a,List.map aux lst, b)
  | Lstaticraise (a,lst) ->
     Lstaticraise (a,List.map aux lst)
  | Lifthenelse (i,f,e) ->
     Lifthenelse (aux i, aux f, aux e)
  | Lsequence (l,r) ->
     Lsequence (aux l, aux r)
  | Lwhile (l,r) ->
     Lwhile (aux l, aux r)
  | Lifused (i,l) ->
     Lifused (i, aux l)
  | Lswitch (l,s,i) ->
     let sw_consts = insnd s.sw_consts in
     let sw_blocks = insnd s.sw_blocks in
     Lswitch (aux l, {s with sw_consts; sw_blocks}, i)
  | Lstringswitch (l,lst,opt,e) ->
     Lstringswitch (aux l, insnd lst, inopt opt, e)
  | Lassign (i,l) ->
     Lassign (i, aux l)
  | Levent (l,e) ->
     Levent (aux l, e)
  | Lstaticcatch (l,lst,r) ->
     Lstaticcatch (aux l, lst, aux r)
  | Ltrywith (l,i,r) ->
     Ltrywith (aux l, i, aux r)
  | Lfor (e,a,b,d,c) ->
     Lfor (e, aux a, aux b, d, aux c)
  | Lsend (a,b,c,d,e) ->
     Lsend (a, aux b, aux c, List.map aux d, e)
  in aux

(* Replace every occurence of ident by its body *)
let replace ident body =
  let lvar x =
    if x = ident
    then body
    else Lvar x in
  let llet aux a b c d e = Llet (a,b,c,aux d,aux e) in
  fold_lambda lvar llet

(* Is the definition inlineable ? *)
let inlineable x f =
  match x with
  | Alias -> true
  | Strict ->
     begin
       match f with
       | Lvar _ | Lconst _ -> true
       | _ -> false
     end
  | _  -> false

(* Inline all possible "let definitions"
   (that is, all "let definitions" without a side effet) *)
let inline_all =
  let lvar x = Lvar x in
  let llet aux k e ident l r =
    if inlineable k l
    then
      aux (replace ident l r)
    else
      Llet (k, e, ident, aux l, aux r) in
  fold_lambda lvar llet
end

let filter_map f xs =
  let aux x acc =
    match f x with
    | None -> acc
    | Some x -> x::acc
  in List.fold_right aux xs []

let get_name_of_pat pat =
  match pat.pat_desc with
  | Tpat_var(id, _) -> Some id
  | Tpat_alias(_, id, _) -> Some id
  | _ -> None

let lambda_of_expression expr =
  Lambda_utils.inline_all @@
    Simplif.simplify_lambda @@
      Translcore.transl_exp ~scopes:[] expr

let rec read_module_expr prefix m =
  match m.mod_desc with
  | Tmod_structure structure -> read_structure prefix structure
  | Tmod_functor (_,m) -> read_module_expr prefix m
  | _ -> []

and read_value_binding prefix x =
  match get_name_of_pat x.vb_pat with
  | Some name -> Some
                   (prefix ^ "." ^ (Ident.name name), name, lambda_of_expression x.vb_expr)
  | None -> None

and read_item_desc prefix x =
  let read_module_expr m =
    read_module_expr (prefix ^ "." ^ Option.fold ~none:"" ~some:Ident.name m.mb_id) m.mb_expr in
  match x.str_desc with
  | Tstr_value (_,xs) -> filter_map (read_value_binding prefix) xs
  | Tstr_module m -> read_module_expr m
  | Tstr_recmodule xs ->
     List.flatten @@
       List.map read_module_expr xs
  | _ -> []

and read_structure prefix structure =
  List.flatten @@
    List.map (read_item_desc prefix) structure.str_items

let tool_name = "ocamlopt"

let with_info =
  Compile_common.with_info ~native:true ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

let (|>>) (x, y) f = (x, f y)

(** Native compilation backend for .ml files. *)

let flambda i backend typed =
  if !Clflags.classic_inlining then begin
    Clflags.default_simplify_rounds := 1;
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    Clflags.unbox_free_vars_of_closures := false;
    Clflags.unbox_specialised_args := false
  end;
  typed
  |> Profile.(record transl)
      (Translmod.transl_implementation_flambda i.module_name)
  |> Profile.(record generate)
    (fun {Lambda.module_ident; main_module_block_size;
          required_globals; code } ->
    ((module_ident, main_module_block_size), code)
    |>> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
    |>> Simplif.simplify_lambda
    |>> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
    |> (fun ((module_ident, main_module_block_size), code) ->
      let program : Lambda.program =
        { Lambda.
          module_ident;
          main_module_block_size;
          required_globals;
          code;
        }
      in
      Asmgen.compile_implementation
        ~backend
        ~filename:i.source_file
        ~prefixname:i.output_prefix
        ~middle_end:Flambda_middle_end.lambda_to_clambda
        ~ppf_dump:i.ppf_dump
        program);
    Compilenv.save_unit_info (cmx i))

type extern_flags = unit

external to_channel: out_channel -> 'a -> extern_flags list -> unit
  = "caml_output_value"

let rec get_after_build = function
  | [] | [_] -> "ANON"
  | x::(y::_ as lst) ->
     if x = "build"
     then y
     else get_after_build lst

let get_pkg_name pwd =
  let xs = String.split_on_char '/' pwd in
  get_after_build xs

let export_lambda pwd source_file module_name ftyped =
  try
    let prefix = Sys.getenv "ASAK_PREFIX" in
    let pkg_name = get_pkg_name pwd in
    let source_file = String.map (fun x -> if x = '/' then '.' else x) source_file in
    let read = read_structure module_name ftyped in
    let outchan =
      open_out_bin (prefix ^ pkg_name ^ ":" ^ source_file ^ ":" ^ module_name) in
    to_channel outchan read [];
    close_out outchan
  with | Not_found -> ()

let clambda i backend typed =
  Clflags.use_inlining_arguments_set Clflags.classic_arguments;
  export_lambda (Sys.getcwd ()) i.source_file i.module_name (fst typed);
  typed
  |> Profile.(record transl)
    (Translmod.transl_store_implementation i.module_name)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |> Asmgen.compile_implementation
            ~backend
            ~filename:i.source_file
            ~prefixname:i.output_prefix
            ~middle_end:Closure_middle_end.lambda_to_clambda
            ~ppf_dump:i.ppf_dump;
       Compilenv.save_unit_info (cmx i))

let implementation ~backend ~source_file ~output_prefix =
  let backend info typed =
    Compilenv.reset ?packname:!Clflags.for_package info.module_name;
    if Config.flambda
    then flambda info backend typed
    else clambda info backend typed
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  Compile_common.implementation info ~backend
