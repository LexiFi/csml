(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

(* Stub generation *)

(* To be fixed: all-float records are not properly supported! *)


let stubclass = ref "###"

open Misc
open Ast
open Tast
open Pp
open Pp.Operators

let comma f l = list ~sep:~~"," f l

let make_relative pr ml =
  let last = ref 0 in
  begin try for i = 0 to (min (String.length pr) (String.length ml)) - 1 do
    if ml.[i] <> pr.[i] then raise Exit;
    if pr.[i] = '.' then last := (i + 1)
  done with Exit -> () end;
  String.sub ml !last (String.length ml - !last)


(* Print ML types *)

let rec mltyp = function
  | Int -> ~~"int"
  | Int64 -> ~~"int64"
  | Double | Single -> ~~"float"
  | String | Blob -> ~~"string"
  | Bool -> ~~"bool"
  | Date -> ~~"date"
  | CS2ML_T (_, _, _, PCs_as_int, _) -> ~~"int"
  | Variantized (_, ml, _, _, _)
  | ML2CS_T (_,ml,_) | CS2ML_T (_,_,ml,_,_) -> ~~"%s" ml
  | Tuple tl -> ~~"(%t)" (list ~sep:~~"*" mltyp tl)
  | List t -> ~~"%t list" (mltyp t)
  | Option t | Nullable t -> ~~"%t option" (mltyp t)
  | Array t -> ~~"%t array" (mltyp t)
  | Arrow (args,res) -> ~~"(%t)" (mlarrow args res)
  | Exn -> ~~"exn"
  | IntPtr -> ~~"Obj.t"
  | Variant -> ~~"Mlfi_isdatypes.variant"
  | Weak (_, t) -> mltyp t

and mlarrow args res =
  ~~"%t -> %t" (mlargs args) (mlres res)

and mlargs = function
  | [] -> ~~"unit"
  | args -> list ~sep:~~" -> " mltyp args

and mlres = function
  | None -> ~~"unit"
  | Some t -> mltyp t

(* Print C# types *)

let rec cstyp = function
  | String -> ~~"string"
  | Blob -> ~~"byte[]"
  | Double -> ~~"double"
  | Single -> ~~"float"
  | Int -> ~~"int"
  | Int64 -> ~~"long"
  | Bool -> ~~"bool"
  | Date -> ~~"DateTime"
  | Variantized (s, _, _, _, _)
  | CS2ML_T (s,_,_,_,_) | ML2CS_T (s,_,_) -> ~~"%s" s
  | IntPtr -> ~~"IntPtr"
  | Tuple tl -> ~~"%s.Tuple<%t>" ns (comma cstyp tl)
  | List t -> ~~"List<%t>" (cstyp t)
  | Array t -> ~~"%t[]" (cstyp t)
  | Option t -> ~~"%s.Option<%t>" ns (cstyp t)
  | Nullable t -> cstyp t
  | Arrow (args,None) -> ~~"%s.ArrowVoid%t" ns (generics args)
  | Arrow (args,Some res) -> ~~"%s.Arrow%t" ns (generics (args@[res]))
  | Exn -> ~~"Exception"
  | Variant -> ~~"LexiFi.Interop.Variants.Variant"
  | Weak (_, t) -> cstyp t

and generics = function
  | [] -> empty
  | l -> ~~"<%t>" (comma cstyp l)

let csres = function
  | None -> ~~"void"
  | Some t -> cstyp t

let string_of_cstyp t =
  to_string ({width=1000;indent=0},cstyp t)

let xis n = seq ~sep:~~"," ~~"x%i" 0 n
let csargs = listi ~sep:~~"," (fun i t -> ~~"%t x%i" (cstyp t) i)
let csargs_l = seq ~sep:~~"," ~~"IntPtr x%i" 0
let name_args = mapi (fun i t -> (Printf.sprintf "x%i" i, Printf.sprintf "y%i" i,t))

(* C# -> ML stubs *)

let cs2ml_stubs idl =
  let r = ref [] in
  let value id (args, res) k =
    match k.cs_kind with
    | Kill | IsNull | Null | MLCode _ -> ()
    | _ ->  r := `Value (id, args, res, k) :: !r
  in
  let rec decl = function
  | PModule (_,sub) -> decls sub
  | PType (id, _, PCs_enum (constrs,poly), cs, _) ->
      let constrs =
        if not poly then mapi (fun i c -> (c, i)) constrs
        else
          List.map (fun c ->
            (c, Obj.magic (CamlinternalOO.public_method_label c))) constrs
      in
      r := `Enum (id, cs, constrs) :: !r
  | PType (_,_,(PCs_abstract | PCs_as_int), _, _) -> ()
  | PType (_,_,PCs_mlclass {contents=None; _}, _, _) -> assert false
  | PType (_,_,PCs_mlclass {contents=Some (inh, meths); _}, cs1, _) ->
      begin match inh with
      | Some (CS2ML_T (cs2, _, _, _, _)) -> r := `Subtype (cs1, cs2) :: !r
      | None -> ()
      | _ -> assert false
      end;
      List.iter (function PMLClass_method (id, _, arr, k) -> value id arr k | PMLClass_inline _ -> ()) meths
  | PValue (id, _, arr, k) -> value id arr k
  | PMLInline _ -> ()
  and decls l = List.iter decl l
  in
  List.iter (fun (_,_,d) -> decls d) idl.tml;
  List.rev !r

let method_type (args, res) =
  let res = match res with Some t -> mltyp t | None -> ~~"unit" in
  match args with
  | _::[] -> res
  | _::args -> ~~"%t -> %t" (list ~sep:~~" -> " mltyp args) res
  | [] -> assert false

let print_ml_import l =
  let value iname id n (args, res) k =
    let typ = ~~"%t -> %t" (mlargs args) (mlres res) in
    match args, res, k with
    | [CS2ML_T (_,_,_,PCs_abstract,_)], None, {cs_kind=Kill; _} -> ~~"let %s : %t = fun x -> Obj.magic (!Csml_iface.release_cs_value) x" iname typ
    | [CS2ML_T (_,_,_,PCs_mlclass _,_)], None, {cs_kind=Kill; _} -> ~~"let %s : %t = fun x -> Obj.magic (!Csml_iface.release_cs_value) (x#cshandle)" iname typ

    | [CS2ML_T (_,_,_,PCs_abstract,_)], Some Bool, {cs_kind=IsNull; _} -> ~~"let %s : %t = fun x -> Obj.magic x == ()" iname typ
    | [CS2ML_T (_,_,_,PCs_mlclass _,_)], Some Bool, {cs_kind=IsNull; _} -> ~~"let %s : %t = fun x -> Obj.magic (x#cshandle) == ()" iname typ

    | [], Some (CS2ML_T (_,_,id,PCs_mlclass _,_)), {cs_kind=Null; _} -> ~~"let %s : %t = let null = lazy ((Csml_iface.ml2cs_registered %S : unit -> _) ()) in fun () -> Lazy.force null" iname typ id
    | [], Some (CS2ML_T (_,_,_,PCs_abstract,_)), {cs_kind=Null; _} -> ~~"let %s : %t = fun () -> Obj.magic ()" iname typ
    | _, _, {cs_kind=MLCode s; _} -> ~~"let %s : %t = (%s)" iname typ s
    | _, _, {cs_kind=(Kill|IsNull|Null);cs_loc=loc; _} -> Lexer.error loc "The type is not compatible with this kind of C# component"
    | _ -> ~~"let %s : (%t) = Csml_iface.find_cs2ml_callback %i %S %S" iname typ (max 1 (List.length args)) id n
  in

  let rec decl = function
  | PModule (n, sub) ->  ~~"module %s = struct\n @[%t@]\n end" n (decls sub)
  | PType (id, n, (PCs_abstract | PCs_enum (_,true)), _, _) -> ~~"type %s = %s" n id
  | PType (_, n, PCs_as_int, _, _) -> ~~"type %s = int" n
  | PType (_, _, PCs_mlclass {contents=None; _}, _, _) -> assert false
  | PType (id, n, PCs_mlclass {contents=Some (inh, meths); _}, _, pr) ->
      let bind = function
        | PMLClass_method (id, n, arr, k) ->
            begin match k.cs_kind with Kill | IsNull | Null | MLCode _ -> empty | _ -> value id id n arr k ++ ~~"\n " end
        | PMLClass_inline _ -> empty
      in
      let f = function
        | PMLClass_method (methid, n, (args, res) , k) ->
            begin match args, res, k.cs_kind with
            | [CS2ML_T _], None, Kill -> ~~"method %s : unit = Obj.magic (!Csml_iface.release_cs_value) (this#cshandle)\n " n
            | [CS2ML_T _], Some Bool, IsNull -> ~~"method %s = Obj.magic (this#cshandle) == ()" n
            | _, _, MLCode s -> ~~"method %s = let this = (this :> %s) in ignore this; (%s)\n " n id s
            | _, _, (Kill|IsNull|Null) -> Lexer.error k.cs_loc "The type is not compatible with this kind of C# component"
            | arg :: _, _, _ -> ~~"method %s = %s (this :> %t)\n " n methid (mltyp arg)
            | [], _, _ -> assert false
            end
        | PMLClass_inline (n, t, impl) -> ~~"method %s : %s = (%s) \n " n t impl
      in
      let inh = match inh with
      | None -> ~~"Csml_iface.csval"
      | Some (CS2ML_T (_, ml, _, _, _)) -> ~~"%s" (make_relative pr ml)
      | _ -> assert false
      in
      all (List.map bind meths)
        ++ ~~"class %s handle = object(this)\n @[initializer ignore this\n inherit %t handle\n method is_%s = ()\n %t@] end\n " n
        inh
        id (all (List.map f meths))
        ++ ~~"let () = Csml_iface.ml2cs_register %S (new %s : Csml_iface.cshandle -> %s)\n " id n id
        ++ ~~"class type %s_cltype = object\n @[inherit %t\n method is_%s: unit\n %t@] end\n "
        n inh id (all (List.map
                         (function
                           | PMLClass_method (_, n, t, _) -> ~~"method %s: %t\n " n (method_type t)
                           | PMLClass_inline (n, t, _) -> ~~"method %s: %s\n " n t
                         ) meths))
  | PType (id, n, PCs_enum (constrs, false), _, _) ->
      ~~"type %s = %s = %s" n id (String.concat " | " constrs)
  | PValue (id, n, arr, k) ->
      value n id n arr k
  | PMLInline s -> ~~"%s\n " s
  and decls l = list ~sep:ws decl l
  in
  decls l

(* C# <-> ML value translation *)

let check_caml_exn res =
  ~~"if (((long) %s & 3) == 2) throw %s.MLException.Create((IntPtr)((long)%s & ~3));\n" res ns res

let cs2ml_simple = function
  | ML2CS_T (_, _, PAbstract) | Int | Bool | IntPtr | Date
  | CS2ML_T (_, _, _, (PCs_as_int | PCs_enum _), _) -> true
  | _ -> false

module StringMap =
  Map.Make(struct
    type t = string
    let compare = compare
  end)


let may_allocate_memo = ref StringMap.empty

let rec ml2cs_may_allocate = function
  | Weak (_, t) | List t | Option t | Nullable t -> ml2cs_may_allocate t
  | Array t -> (match t with Single | Double | ML2CS_T (_, _, PAbstract) -> true | _ -> ml2cs_may_allocate t)
  | Tuple tl -> List.exists ml2cs_may_allocate tl
  | Int
  | Bool
  | Date
  | Int64
  | Double | Single
  | String
  | Blob
  | CS2ML_T _
  | Arrow _
  | Exn | IntPtr | Variant
  | ML2CS_T (_, _, PAbstract) -> false
  | ML2CS_T (cs, _, ((PRecordType _ | PSumType _) as d)) -> ml2cs_may_allocate_datatype cs d
  | Variantized _ -> true
and ml2cs_may_allocate_datatype cs d =
  try StringMap.find cs !may_allocate_memo
  with Not_found ->
    let old_memo = !may_allocate_memo in
    may_allocate_memo := StringMap.add cs false old_memo;
    let record = List.exists (fun (_, _, _, t) -> ml2cs_may_allocate (Lazy.force t.contents)) in
    let r = match d with
    | PRecordType (_, fields) -> record fields
    | PSumType (_, constrs) -> List.exists record (List.map (fun (_, _, r) -> r) constrs)
    | _ -> assert false
    in
    if r then may_allocate_memo := StringMap.add cs true old_memo;
    r

let save arg = ~~"%s.csml_push(%s);\n " nss arg
let save_many = function
  | [] -> empty
  | [arg] -> save arg
  | l -> ~~"%s.csml_push_%i(%t);\n " nss (List.length l) (list ~sep:~~"," ~~"%s" l)

let restore arg = ~~"%s = %s.csml_pop();\n " arg nss
let saved_arg = Printf.sprintf "(Marshal.ReadIntPtr(Marshal.ReadIntPtr(%s.mlstack)))" nss
let wrap_save_restore arg doit t =
  if doit then save arg ++ t saved_arg ++ restore arg
  else t arg

let rec ml2cs_val res arg t =
  let sub ?(more=true) i t =
    let arg_p = Printf.sprintf "%s_%i" arg i
    and res_p = Printf.sprintf "%s_%i" res i in
    wrap_save_restore arg (more && ml2cs_may_allocate t)
      (fun arg ->
        ~~"IntPtr %s = Marshal.ReadIntPtr(%s,%i * IntPtr.Size);\n " arg_p arg i
          ++ ml2cs_val res_p arg_p t
      )

  in
  let decl = ~~"%t %s = " (cstyp t) res in
  let declnew = ~~"%t %s = new %t" (cstyp t) res (cstyp t) in
  match t with
  | Weak (loc, _) -> Lexer.error loc.contents "weak not supported for ML -> C# values"
  | Int -> ~~"%t((int)%s >> 1);\n " decl arg
  | Bool -> ~~"%t(((uint)%s >> 1) != 0);\n " decl arg
  | Date -> ~~"%t%s.csml_of_mldate(%s);\n " decl nss arg
  | Int64 -> ~~"%tcsml_int64_val(%s);\n " decl arg (* TODO: direct access *)
  | Double -> ~~"%t%s.csml_double_val(%s);\n " decl nss arg
      (* TODO: direct access *)
  | Single -> ~~"%t(float) %s.csml_double_val(%s);\n " decl nss arg
      (* TODO: direct access *)
  | String -> ~~"%t%s.csml_copy_string(%s);\n " decl nss arg
  | Blob -> ~~"%t%s.csml_copy_blob(%s);\n " decl nss arg
  | ML2CS_T (_, _, (PAbstract | PRecordType _)) -> ~~"%t(%s);\n " declnew arg
  | ML2CS_T (cs, _, PSumType _) -> ~~"%t%s.csml_of_ml(%s);\n " decl cs arg
  | CS2ML_T (cs, _, _, PCs_as_int, _) -> ~~"%t(%s)((int)%s >> 1);\n " decl cs arg
  | CS2ML_T (cs, _, _, (PCs_abstract | PCs_mlclass _), _) -> ~~"%t(%s)%s.csml_get_csval(%s);\n " decl cs nss arg
  | CS2ML_T (cs, _, id, PCs_enum _, stubclass) -> ~~"%t(%s)%s.%s.csml_int_to_enum_%s((int)%s >> 1);\n " decl cs ns stubclass id arg
  | List t ->
      ~~"%t();\n " declnew
        ++ ~~"while ((long) %s != 1) {\n @[%t@]}\n " arg
        (
         sub 0 t
           ++ ~~"%s.Add(%s_0);\n " res res
           ++ ~~"%s = Marshal.ReadIntPtr(%s, IntPtr.Size);\n " arg arg
        )
  | Array t ->
      (* TODO: direct access for float arrays *)
      let maybefloat = match t with Single | Double | ML2CS_T (_, _, PAbstract) -> true | _ -> false in
      let arg_p = Printf.sprintf "%s_x" arg
      and res_p = Printf.sprintf "%s_y" res
      and i = Printf.sprintf "%s_i" arg
      and len = Printf.sprintf "%s_len" arg in
      let mayalloc = maybefloat || ml2cs_may_allocate t in
      let rec traverse_arrays accu = function
        | Array t -> traverse_arrays (accu ^ "[]") t
        | t -> t, accu in
      let base_t, array_dims = traverse_arrays "" t in
      (if maybefloat then ~~"int %s = %s.csml_array_length(%s);\n " len nss arg
      else ~~"int %s = ((int)Marshal.ReadIntPtr(%s,-IntPtr.Size)) >> 10;\n " len arg)  (* Check 64-bit *)
        ++ ~~"%tnew %t[%s]%s;\n " decl (cstyp base_t) len array_dims
        ++ wrap_save_restore arg mayalloc
        (fun arg ->
          ~~"for (int %s = 0; %s < %s; %s++) {\n @[%t@]}\n " i i len i
            (
             (if maybefloat then
               ~~"IntPtr %s = %s.csml_array_get(%s,%s);\n " arg_p nss arg i
             else
               ~~"IntPtr %s = Marshal.ReadIntPtr(%s,IntPtr.Size * %s);\n " arg_p arg i
             )
               ++ ml2cs_val res_p arg_p t
               ++ ~~"%s[%s] = %s;\n " res i res_p
            )
        )
  | Tuple tl ->
      listi (sub ~more:true) tl ++
        ~~"%t(%t);\n " declnew (listi ~sep:~~"," (fun i _ -> ~~"%s_%i" res i) tl)
  | Option t ->
      ~~"%t();\n " declnew
      ++ ~~"if ((long) %s != 1) {\n @[%t@]}\n " arg
        (sub ~more:false 0 t ++ ~~"%s.Val = %s_0;\n " res res)
  | Nullable t ->
      ~~"%tnull;\n " decl
      ++ ~~"if ((long) %s != 1) {\n @[%t@]}\n " arg
        (sub ~more:false 0 t ++ ~~"%s = %s_0;\n " res res)
  | Arrow (args,ret) ->
      let id = Resolution.find_ml2cs_arrow_type (args, ret) in
      ~~"%t%s.%s.Closure_%s.ml2cs(%s);\n " decl ns !stubclass id arg
  | Exn -> ~~"%tMLException.Create(%s);\n " decl arg
  | IntPtr -> ~~"%t%s;\n " decl arg
  | Variant -> ~~"%tLexiFi.Interop.Variants.MLFi_dll.VariantFromIntPtr(%s);\n " decl arg
  | Variantized (cs, _, to_var, _, stubclass) -> ~~"%t%s.extract(LexiFi.Interop.Variants.MLFi_dll.VariantFromIntPtr(%s.%s.ml2cs_stub_%s(%s)));\n" decl cs ns stubclass to_var arg

and push_cs2ml_val arg = function
  | ML2CS_T (_, _, PAbstract) | Int | Bool | IntPtr | Date | CS2ML_T (_, _, _, PCs_enum _, _) | CS2ML_T (_, _, _, PCs_as_int, _) -> empty
  | ML2CS_T (_, _, (PSumType _ | PRecordType _)) -> ~~"%s.csml_push();\n " arg
  | Weak (_, CS2ML_T (_, _, _, PCs_abstract, _)) -> ~~"%s.csml_push_cs_value(%s, GCHandleType.Weak);\n " nss arg
  | CS2ML_T (_, _, _, PCs_abstract, _) -> ~~"%s.csml_push_cs_value(%s, GCHandleType.Normal);\n " nss arg
  | Weak (_, CS2ML_T (_, _, id, PCs_mlclass _, stubclass)) -> ~~"%s.csml_push(%s.%s.ml2cs_stub_%s(%s.csml_cs_value(%s, GCHandleType.Weak)));\n " nss ns stubclass id nss arg
  | CS2ML_T (_, _, id, PCs_mlclass _, stubclass) -> ~~"%s.csml_push(%s.%s.ml2cs_stub_%s(%s.csml_cs_value(%s, GCHandleType.Normal)));\n " nss ns stubclass id nss arg
  | Double | Single -> ~~"%s.csml_create_double(%s);\n " nss arg
  | Int64 -> ~~"%s.csml_create_int64(%s);\n " nss arg
  | String -> ~~"%s.csml_push_string(%s);\n " nss arg
  | Blob -> ~~"%s.csml_push_blob(%s);\n " nss arg
  | List t ->
      let x = arg ^ "_p" in
      ~~"bool %s_first = true;\n " arg
        ++ ~~"foreach(%t %s in %s) {\n @[%t@]}\n " (cstyp t) x arg
        (~~"  %s.csml_create(0,2);\n " nss
           ++ cs2ml_setfield 0 x t
           ++ ~~"if (%s_first) { %s.csml_dup(); %s_first = false; } else %s.csml_setfield_swap(1);\n " arg nss arg nss
        )
        ++ ~~"if (%s_first) %s.csml_push((IntPtr)1);\n " arg nss
        ++ ~~"else { %s.csml_setfield_imm(1,(IntPtr)1); %s.csml_pop(); }\n " nss nss
  | Array t ->
      let x = arg ^ "_p" in
      let i = arg ^ "_i" in
      ~~"if (%s.Length == 0) %s.csml_create(0,0);\n " arg nss
      ++ ~~"else for (int %s = 0; %s < %s.Length; %s++) {\n @[%t@]}\n " i i arg i
        (~~"%t %s = %s[%s];\n " (cstyp t) x arg i
           ++ cs2ml_force_push x t
           ++ ~~" if (%s == 0) %s.csml_push_array(%s.Length);\n " i nss arg
           ++ ~~" else %s.csml_array_set(%s);\n " nss i
        )
  | Option t ->
      let x = arg ^ "_v" in
      ~~"if (%s.Is_some) {\n @[%t@]}\n " arg
        (~~"%s.csml_create(0,1);\n " nss
           ++ ~~"%t %s = %s.Val;\n " (cstyp t) x arg
           ++ cs2ml_setfield 0 x t
        )
        ++ ~~"else %s.csml_push((IntPtr)1);\n " nss
  | Nullable t ->
      ~~"if (%s != null) {\n @[%t@]}\n " arg
        (~~"%s.csml_create(0,1);\n " nss
           ++ cs2ml_setfield 0 arg t
        )
        ++ ~~"else %s.csml_push((IntPtr)1);\n " nss
  | Tuple tl ->
      ~~"%s.csml_create(0,%i);\n " nss (List.length tl)
        ++ listi
        (fun i t ->
          let xi = Printf.sprintf "%s_%i" arg i in
          ~~"%t %s = %s.TVal%i;\n " (cstyp t) xi arg i
            ++ cs2ml_setfield i xi t
        ) tl
  | Arrow (args, res) ->
      let id = Resolution.find_cs2ml_arrow_type (args, res) in
      ~~"if (%s.Target is %s.Callback) ((%s.Callback)%s.Target).cs2ml();\n " arg ns ns arg
        ++ ~~"else %s.csml_push(%s.%s.ml2cs_stub_%s(%s.csml_cs_value(%s, GCHandleType.Normal)));\n " nss ns !stubclass id nss arg
  | Weak (_, Arrow (args, res)) ->
      let id = Resolution.find_cs2ml_arrow_type (args, res) in
      ~~"if (%s.Target is %s.Callback) ((%s.Callback)%s.Target).cs2ml();\n " arg ns ns arg
        ++ ~~"else %s.csml_push(%s.%s.ml2cs_stub_%s(%s.csml_cs_value(%s, GCHandleType.Weak)));\n " nss ns !stubclass id nss arg
  | Exn ->
      ~~"%s.csml_push_exception(%s);\n " nss arg
  | Variant -> ~~"%s.Mlfi();\n" arg
  | Variantized (_, _, _, of_var, stubclass) -> ~~"%s.ToVariant().Mlfi(); %s.csml_push(%s.%s.ml2cs_stub_%s(%s.csml_pop()));\n" arg nss ns stubclass of_var nss
  | Weak (loc, _) -> Lexer.error loc.contents "Weak (C# -> ML) only supported for functions and abstract values"

and cs2ml_setfield i x t =
  if cs2ml_simple t then ~~"%s.csml_setfield_imm(%i, %t);\n " nss i (get_cs2ml_val x t)
  else push_cs2ml_val x t ++ ~~ "%s.csml_setfield(%i);\n " nss i

and cs2ml_force_push x t =
  if cs2ml_simple t then ~~"%s.csml_push(%t);\n " nss (get_cs2ml_val x t)
  else push_cs2ml_val x t

and get_cs2ml_val arg = function
  | ML2CS_T (_, _, PAbstract) -> ~~"%s.csml_get_mlval(%s.handle)" nss arg
        (* TODO: if this call raises an exception, one should clear the current
           stack of ML values... *)
  | Int -> ~~"(IntPtr)((%s << 1) + 1)" arg
  | CS2ML_T (_, _, _, PCs_as_int, _) -> ~~"(IntPtr)(((long)%s << 1) + 1)" arg
  | Bool -> ~~"(IntPtr) (%s ? 3 : 1)" arg
  | Date -> ~~"%s.csml_to_mldate(%s)" nss arg
  | CS2ML_T (_, _, id, PCs_enum _, stubclass) -> ~~"(IntPtr)((%s.%s.csml_enum_to_int_%s(%s) << 1) + 1)" ns stubclass id arg
  | String | Blob | CS2ML_T _ | List _ | Tuple _ | Option _ | Nullable _
  | Double | Single | Int64 | Arrow _ | Exn | Array _ | Variant | Variantized _
  | ML2CS_T (_, _ , (PSumType _ | PRecordType _))
  | Weak _ -> ~~"%s.csml_pop()" nss
  | IntPtr -> ~~"%s" arg

and cs2ml_args args =
  list (fun (cs,_,t) -> push_cs2ml_val cs t) args
    ++ list (fun (cs,ml,t) -> cs2ml_val ml cs t) (List.rev args)

and cs2ml_res cs = function
  | None -> empty
  | Some t -> let ml = cs^"_ml" in ml2cs_val ml cs t ++ ~~"return %s;\n " ml

and cs2ml_val res arg t = ~~"IntPtr %s = %t;\n " res (get_cs2ml_val arg t)

let cs2ml_cs_stub_value (id, args,res, {cs_kind=k;cs_ignore=ign;cs_loc=loc}) =
  begin match (res = None && ign = false), k with
  | true, Static s ->
      (* Use a delegate type to enforce that the method returns void. *)
      ~~"public delegate %t cs2ml_styp_%s(%t);\n " (csres res) id (csargs args)
        ++ ~~"private static void check_%s() { cs2ml_styp_%s x = new cs2ml_styp_%s(%s); }\n "
        id id id s  (* TODO: reuse ArrowVoid<...> delegate? *)
  | true, Instance s ->
      ~~"public delegate %t cs2ml_styp_%s(%t);\n " (csres res) id (csargs (List.tl args))
        ++ ~~"private static void check_%s(%t y) { cs2ml_styp_%s x = new cs2ml_styp_%s(y.%s); }\n "
        id (cstyp (List.hd args)) id id s
  | _ -> empty
  end
    ++ let argsi = name_args args in
    let parg (_,cs,_) = ~~"%s" cs in
    let pargs = comma parg in
    let access = function "this" -> empty | s -> ~~".%s" s in
    let body = match k, res, argsi with
    | Instance s, _, arg0::args -> ~~"%t%t(%t)" (parg arg0) (access s) (pargs args)
    | Static s, _, _ -> ~~"%s(%t)" s (pargs argsi)
    | Ctor, Some _, _ -> ~~ "new %t(%t)"  (csres res) (pargs argsi)

    | SetProp s, None, [arg0;arg1] -> ~~"%t%t = %t" (parg arg0) (access s) (parg arg1)
    | GetProp s, Some _, [arg0] -> ~~"%t%t" (parg arg0) (access s)
    | OpProp (s, op), None, [arg0; arg1] -> ~~"%t%t %s= %t" (parg arg0) (access s) op (parg arg1)
    | SetItemProp s, None, [arg0;arg1;arg2] -> ~~"%t%t[%t] = %t" (parg arg0) (access s) (parg arg1) (parg arg2)
    | GetItemProp s, Some _, [arg0; arg1] -> ~~"%t%t[%t]" (parg arg0) (access s) (parg arg1)

    | StaticSetProp s, None, [arg0] -> ~~"%s = %t" s (parg arg0)
    | StaticGetProp s, Some _, [] -> ~~"%s" s
    | StaticSetItemProp s, None, [arg0; arg1] -> ~~"%s[%t] = %t" s (parg arg0) (parg arg1)
    | StaticGetItemProp s, Some _, [arg0] -> ~~"%s[%t]" s (parg arg0)
    | StaticOpProp (s, op), None, [arg0] -> ~~"%s %s= %t" s op (parg arg0)

    | Cast, Some _, [arg0] -> ~~"%t as %t" (parg arg0) (csres res)
    | (Kill | IsNull | MLCode _ | Ctor), _, _ -> assert false
    | _ -> Lexer.error loc "The type is not compatible with this kind of C# component"
    in
    let body =
      match res with
      | Some t ->
          ~~ "%t yr = %t;\n " (cstyp t) body
            ++ push_cs2ml_val "yr" t
            ++ cs2ml_val "xr" "yr" t
            ++ ~~ "return xr;\n "
      | None -> ~~"%t;\n " body ++ ~~"return (IntPtr)1;\n "
    in
    let nargs = max 1 (List.length args) in
    let need_save = List.exists (fun (_, _, t) -> ml2cs_may_allocate t) argsi in
    ~~ "private static IntPtr cs2ml_stub_%s(%t) {\n @[%t@]}\n " id (csargs_l nargs)
      (~~"try {\n @[%t@]}\n "
         (
          (if need_save then save_many (List.map (fun (ml, _, _) -> ml) argsi) else empty)
            (* We cannot simply call save many times since it could trigger the GC. *)
            ++  list (fun (ml, cs, t) -> (if need_save then restore ml else empty) ++ ml2cs_val cs ml t) argsi
            ++  body
         )
         ++ ~~"catch (Exception exn) { return (IntPtr)(2 | (long)%s.csml_create_exception(exn)); }\n " nss
      )
      ++ ~~"private static Csml.cs2ml_typ%i cs2ml_fptr_%s = null;\n " nargs id

let cs2ml_cs_enum (id, cs, constrs) =
  ~~ "public static %s csml_int_to_enum_%s(int i) {\n @[switch (i) {\n %tdefault: return (%s)0;\n }@]\n }\n "
    cs id
    (list (fun (c, i) -> ~~"case %i: return %s.%s;\n " i cs c) constrs)
    cs
    ++
    ~~ "public static int csml_enum_to_int_%s(%s x) { @[switch (x) {\n %tdefault: return 0;\n }@]\n }\n "
    id cs
    (list (fun (c, i) -> ~~"case %s.%s: return %i;\n " cs c i) constrs)

(* TODO: generate in the init code some reflection to check that the enum is
   exhaustive. *)

let print_cs2ml_cs_stubs l = list
    (function
      | `Value v -> cs2ml_cs_stub_value v
      | `Enum t -> cs2ml_cs_enum t
      | `Subtype (t1, t2) ->
          ~~ "public static %s check_subtyping(%s x) { return x; }\n " t2 t1
    ) l

(* ML -> C# stubs *)

let force_record = List.map (fun (mut, idml, idcs, t) -> (mut, idml, idcs, Lazy.force t.contents))

let ml_record_wrapper over id args tag =
  let args = force_record args in
  let typs = List.map (fun (_, _, _, t) -> t) args in
  let from_ml =
    if args = [] then empty
     else
      ~~"public %s(IntPtr v) {\n @[%t@]}\n " id
        (listi
           (fun i t ->
             let ml = Printf.sprintf "v_%i" i in
             all
               [
                wrap_save_restore "v" (ml2cs_may_allocate t)
                  (fun v ->
                    ~~"IntPtr %s = Marshal.ReadIntPtr(%s,IntPtr.Size * %i);\n " ml v i ++
                      ml2cs_val (Printf.sprintf "val%i_" i) ml t
                  );
                ~~"val%i = val%i_;\n " i i;
              ]) typs
        )
  in
  let to_ml =
    if args = [] then
      ~~"%s.csml_push((IntPtr)%i);\n " nss (tag * 2 + 1)
    else
      ~~"%s.csml_create(%i,%i);\n " nss tag (List.length typs)
        ++ listi
        (fun i t ->
          cs2ml_setfield i (Printf.sprintf "val%i" i) t
        ) typs
  in
  all
    [
     ~~"public %s(%t) {\n @[%t@]}\n "
       id
       (csargs typs)
       (listi (fun i _ -> ~~"val%i = x%i;\n " i i) args);
     from_ml;
     ~~"%spublic void csml_push() {\n @[%t@]}\n " (if over then "override " else "") to_ml;
     listi (fun i t -> ~~"private %t val%i;\n " (cstyp t) i) typs;
     listi
       (fun i (_, _, id, t) ->
         ~~"public %t %s { get { return val%i; } %t}\n "
           (cstyp t) id i
           (~~"set { val%i = value; } " i)
       ) args
   ]

let ml_type_wrapper n = function
  | Some (_, PAbstract) -> all [
      ~~"public IntPtr handle;\n ";
      (* note: the handle can be 0 if an exception was raised in a custom constructor *)
      ~~"~%s() { if ((long)handle != 0) { %s.release_ocaml_value(handle); handle = (IntPtr)0; } }\n " n nss;
      ~~"public %s(IntPtr v) { handle = %s.wrap_ocaml_value(v); }\n " n nss
    ]
  | Some (_, PSumType (_,constrs)) ->
      let constr_class i (_, id, args) =
        ~~"public partial class %s : %s {\n @[%t@]}\n " id n (ml_record_wrapper true id args i)
      in
      let const, nonconst = List.partition (fun (_, _, args) -> args = []) constrs
      in
      let nonconst_dispatch =
        all
          [
           ~~"switch ((long)Marshal.ReadIntPtr(v,-IntPtr.Size) & 0xFF) {\n ";
           listi
             (fun i (_, id, _) -> ~~"case %i: return new %s(v);\n " i id)
             nonconst;
           ~~"default: return null;\n ";
           ~~"}\n ";
         ]
      and const_dispatch =
        all
          [
           ~~"switch ((long)v >> 1) {\n ";
           listi
             (fun i (_, id, _) -> ~~"case %i: return new %s();\n " i id)
             const;
           ~~"default: return null;\n ";
           ~~"}\n ";
         ]
      in
      let dispatch =
        match const, nonconst with
        | [], [] -> assert false
        | [], _ -> nonconst_dispatch
        | _, [] -> const_dispatch
        | _ ->
            ~~ "if (((long) v & 1) == 0)\n @[%t@]\n else\n @[%t@]\n "
              nonconst_dispatch const_dispatch
      in
      let matcher void =
        let cn,t,ret = if void then "MatchVoid","void","" else "Match<__CSMLT__>","__CSMLT__","return " in
        let body = [
          ~~"public %s run(%s x) {\n @[%t@]}\n " t n
            ((list ~sep:~~"else "
                (function
                  | (_, id, []) -> ~~"if (x is %s) %s%s();\n " id ret id
                  | (_, id, args) -> ~~"if (x is %s) { %s y = (%s) x; %s%s(%t); }\n "
                        id id id ret id (comma (fun (_,_,s,_) -> ~~"y.%s" s) args)
                ) constrs)
               ++ ~~"else throw new Exception(\"Match failed\");\n ");
          list
            (fun (_, id, args) -> ~~"abstract public %s %s(%t);\n " t id
                (comma (fun (_,_,s,t) -> ~~"%t %s" (cstyp (Lazy.force t.contents)) s) args))
            constrs;
          ~~"public static %s RunMatch(%s x, %t) {\n @[%t@]}\n " t n
            (listi ~sep:~~","
               (fun n (_, id, args) ->
                 let pargs = comma (fun (_,_,_,t) -> ~~"%t" (cstyp (Lazy.force t.contents))) args in
                 match void, args with
                 | true, [] -> ~~"LexiFi.Interop.ArrowVoid %s%i" id n
                 | true, _ -> ~~"LexiFi.Interop.ArrowVoid<%t> %s%i" pargs id n
                 | false, [] -> ~~"LexiFi.Interop.Arrow<%s> %s%i" t id n
                 | false, _ -> ~~"LexiFi.Interop.Arrow<%t,%s> %s%i" pargs t id n
               ) constrs)
            ((listi ~sep:~~"else " (fun n constr ->
                  match constr with
                  | (_, id, []) -> ~~"if (x is %s) %s %s%i();\n " id ret id n
                  | (_, id, args) -> ~~"if (x is %s) { %s y = (%s) x; %s %s%i(%t); }\n "
                        id id id ret id n (comma (fun (_,_,s,_) -> ~~"y.%s" s) args)
                ) constrs)
               ++ ~~"else throw new Exception(\"Match failed\");\n ");
        ] in
        ~~"public abstract class %s {\n @[%t@]}\n " cn (all body)
      in
      all
        [
         listi constr_class const;
         listi constr_class nonconst;
         ~~"public static %s csml_of_ml(IntPtr v) {\n @[%t@]}\n " n dispatch;
         ~~"public abstract void csml_push();\n ";
         matcher true;
         matcher false;
       ]
  | Some (_, PRecordType (_,fields)) ->
      if List.for_all (fun (_,_,_,t) -> match Lazy.force t.contents with Double | Single -> true | _ -> false) fields then
        Lexer.error (Lexer.fileloc "") "Records of floats are not supported";
      (* Note: we can be tricked if the fields are abstract for us (but known to
         be floats by OCaml. *)
      ml_record_wrapper false n fields 0
  | None -> empty

let declare_ml2cs_stub id nargs =
  let st = Printf.sprintf "ml2cs_stub_%s" id in
  let cache = Printf.sprintf "ml2cs_cache_%s" id in
  ~~"static IntPtr %s = IntPtr.Zero;\n " cache
  ++ ~~"public static IntPtr %s(%t) {\n @[%t@]}\n " st (csargs_l nargs)
    (~~"if (%s == IntPtr.Zero) %s = %s.csml_get_caml_callback(%S);\n " cache cache nss id
     ++ ~~"return %s.csml_apply%i(Marshal.ReadIntPtr(%s)%s%t);\n " nss nargs cache
       (if nargs > 0 then ", " else "")
       (xis nargs)
    )

let iarg (cs,_,t) = ~~"%t %s" (cstyp t) cs
let iargs l = comma iarg l

type csenv = {
    csscope_prefix: string;
    csscope_suffix: string;
  }

let csml_lock body = ~~"lock (LexiFi.Interop.Csml.csml_lock) {\n @[%t@]}\n " body

let print_cs_import l =
  let scope env = function
    | Scope_prefix x -> {env with csscope_prefix = x}
    | Scope_suffix x -> {env with csscope_suffix = x}
  in
  let rec decl env = function
  | PNamespace (n,sub) -> ~~"namespace %s {\n @[%t@]}\n " n (decls env sub)
  | PClass c -> cl env c
  | PCSInline s -> ~~"%s\n " s
  | PCSScope (x, sub) -> decls (scope env x) sub
  and flags f =
    (if f.public then ~~"public " else empty)
    ++ (if f.private_ then ~~"private " else empty)
    ++ (if f.static then ~~"static " else empty)
    ++ (if f.override then ~~"override " else empty)
  and cl env (fl, n, wraps, fields) =
    let abstract =
      match wraps with
      | Some (_, PSumType _) -> "abstract "
      | _ -> ""
    in
    ~~"%s%tpartial class %s {\n @[%t@]}\n " abstract (flags fl) n
      (ml_type_wrapper n wraps ++ list (field env) fields)
  and stub id res args =
    let st = Printf.sprintf "ml2cs_stub_%s" id in
    declare_ml2cs_stub id (List.length args), (st,res,args)
  and body ?(ctor=false) (st,res,args) =
    cs2ml_args args
      ++ ~~ "IntPtr yr = %s(%t);\n " st (targs args)
      ++ check_caml_exn "yr"
      ++
      (if ctor
      then ~~"handle = %s.wrap_ocaml_value(yr);\n " nss
      else cs2ml_res "yr" res
      )

  and targ (_,ml,_) = ~~"%s" ml
  and targs l = comma targ l

  and wrap env body =
    let body =
      match env.csscope_suffix with
      | "" -> body
      | s -> ~~"try {\n @[%t@]}\n finally {%s}\n " body s
    in
    let body =
      match env.csscope_prefix with
      | "" -> body
      | s -> ~~"%s\n " s ++ body
    in
    csml_lock body

  and field env = function
    | PCSScopeClass (x, sub) -> list (field (scope env x)) sub
    | PNestedClass c -> cl env c
    | PMethod (_, fl, n, _, MlKill) ->
        ~~"%t void %s() { %t }\n "
          (flags fl) n
          (wrap env (~~"if ((long)handle != 0) { %s.release_ocaml_value(handle); handle = (IntPtr)0; }" nss))
    | PMethod (id, fl, n, (args, res), _) ->
        let argsi =
          if fl.static then name_args args
          else ("this","ythis",List.hd args) :: (name_args (List.tl args))
        in
        let (dst, st) = stub id res argsi in
        dst
          ++ ~~"%t%t %s(%t) {\n @[%t@]}\n " (flags fl) (csres res) n
          (iargs (if fl.static then argsi else List.tl argsi))
          (wrap env (body st))
    | PConstructor (id,fl,cl,(args,_),_) ->
        assert(fl.static);
        let argsi = name_args args in
        let (dst, st) = stub id None argsi in
        dst
          ++ ~~"%t%s(%t) {\n @[%t@]}\n " (flags {fl with static=false}) cl (iargs argsi)
          (wrap env (body ~ctor:true st))
    | PProperty (fl,n,this,t,get,set) ->
        let th = match this with Some this -> ["this","ythis",this] | None-> []
        and va = ["value","yvalue",t] in
        let r = ref empty in
        let get = optmap (fun (id,_) ->
          let (dst, st) = stub id (Some t) th in r := !r ++ dst; st) get
        and set = optmap (fun (id,_) ->
          let (dst, st) = stub id None (th@va) in r := !r ++ dst; st) set
        in
        !r ++
        ~~"%t%t %s {\n @[%t@]}\n " (flags fl) (cstyp t) n
          (
           let f m st = ~~"%s {\n @[%t@]}\n " m (wrap env (body st)) in
           Pp.opt (f "get") get ++ opt (f "set") set
          )
    | PCSInlineClass s -> ~~"%s\n " s

  and decls env l = list (decl env) l in
  decls {csscope_prefix=""; csscope_suffix=""} l

let ml2cs_stubs idl =
  let r = ref [] in
  let rec decl = function
    | PNamespace (_,sub) -> decls sub
    | PClass c -> cl c
    | PCSInline _ -> ()
    | PCSScope (_,sub) -> decls sub
  and decls l = List.iter decl l
  and cl (_,_,_,fields) = List.iter field fields
  and field = function
    | PMethod (_,_,_,_,MlKill) -> ()
    | PCSScopeClass (_, sub) -> List.iter field sub
    | PNestedClass c -> cl c
    | PConstructor (id,_,_,(args,res),ml)
    | PMethod (id,_,_,(args,res),ml) -> r := (id, args, res, ml) :: !r
    | PProperty (_,_,this,t,get,set) ->
        let th = match this with Some this -> [this] | None -> [] in
        optiter (fun (id,ml) -> r := (id, th, Some t, ml) :: !r) get;
        optiter (fun (id,ml) -> r := (id, th@[t], None, ml) :: !r) set
    | PCSInlineClass _ -> ()
  in
  List.iter (fun (_,d) -> decls d) idl.tcs;
  List.rev !r

let ml2cs_ml_stub (id, args, res, ml) =
  let ml = match ml with
  | MlKill -> assert false
  | Fun s -> ~~"(%s)" s
  in
  ~~"let () = Csml_iface.ml2cs_register %S (%t : %t)\n " id ml (mlarrow args res)

let ml_stubs_mltype = function
  | ML2CS_T (_, ml, PRecordType (priv, fields)) ->
      ~~"type %s = %s = %s{%t}\n " (Resolution.idname ml) ml
        (if priv then "private " else "")
        (list ~sep:~~"; "
           (fun (mut, id, _, t) ->
             let t = Lazy.force t.contents in
             ~~"%t%s:%t" (if mut then ~~"mutable " else empty) id (mltyp t)
           ) fields)
  | ML2CS_T (_, ml, PSumType (priv, constrs)) ->
      ~~"type %s = %s =\n %s@[%t@]\n " (Resolution.idname ml) ml
        (if priv then "private " else "")
        (list
           (function
             | (id, _, []) -> ~~"| %s\n " id
             | (id, _, fields) -> ~~"| %s of %t\n " id
                   (list ~sep:~~" * " (fun (_,_,_,t) -> mltyp (Lazy.force t.contents))
                      fields)
           ) constrs)
  | Variantized (_, ml, to_var, of_var, _) ->
      ~~"let () = Csml_iface.ml2cs_register %S (fun (x : %s) -> try Mlfi_isdatypes.variant x with exn -> Printf.printf \"error while variantizing value of type %s, %%s\" (Printexc.to_string exn); exit 1)\n " to_var ml ml
        ++ ~~"let () = Csml_iface.ml2cs_register %S (fun x -> try (Mlfi_isdatypes.of_variant x : %s) with _exn -> Format.printf \"error while devariantizing value: %%a@.\" Mlfi_isdatypes.print_variant x; exit 1)\n " of_var ml
  | _ -> empty

let print_ml2cs_ml_stubs idl l =
  let arrow (args, _, id) =
    let n = 2 + (max 1 (List.length args)) in
    ~~"let %s: (%t) = Csml_iface.find_cs2ml_callback %i %S %S\n " id
      (seq ~sep:~~"->" (fun _ -> ~~"Obj.t") 0 n) (n - 1) id id
      ++ ~~"let () = Csml_iface.ml2cs_register %S %s\n " id id
  in
  all
    [
     list ~~"open %s\n " idl.idl.opens;
     list arrow idl.cs2ml_arrows;
     list ml2cs_ml_stub l;
     all (List.map ml_stubs_mltype !Resolution.typedecls);
     ~~"let () = Csml_iface.notify_ml_stub %S\n " idl.idl.csid
   ]

let cs_typstub = function
  | Variantized (_, _, to_var, of_var, _) ->
      declare_ml2cs_stub to_var 1 ++
      declare_ml2cs_stub of_var 1
  | CS2ML_T (_, _, id, PCs_mlclass _, _) ->
      declare_ml2cs_stub id 1
  | _ -> empty

let closure_class (args, res, id) =
  let nargs = mapi (fun i t ->
    Printf.sprintf "csarg%i" i,
    Printf.sprintf "mlarg%i" i, t) args
  in
  let body =
    cs2ml_args nargs
      ++ ~~ "IntPtr cbml = Marshal.ReadIntPtr(handle);\n "
      ++ ~~ "IntPtr res = %s.csml_apply%i(%t);\n " nss (List.length args)
      (comma ~~"%s" ("cbml"::(List.map (fun (_,ml,_) -> ml) nargs)))
      ++ check_caml_exn "res"
      ++ cs2ml_res "res" res
  in
  ~~"public class Closure_%s : %s.Callback {\n @[%t@]}\n " id ns
    (all [
     ~~"public Closure_%s(IntPtr x) : base(x) { }\n " id;
     ~~"public %t F(%t) {\n @[%t@]}\n " (csres res) (iargs nargs) (csml_lock body);
     ~~"public static %t ml2cs(IntPtr x) {\n @[return (new Closure_%s(x)).F;@]\n }\n "
       (cstyp (Arrow(args, res))) id
   ])

let print_cs_stub idl cs2ml =
  let cs2ml =
    cs2ml @
    List.map
          (fun (args,res,id) ->
            let t' = CS2ML_T (string_of_cstyp (Arrow (args,res)), "", id,PCs_abstract,!stubclass) in
            let args_ml = t'::(if args = [] then [Int] else args) in
            let k = {cs_kind=Static (nss^".Apply");cs_ignore=false;cs_loc=Lexer.fileloc ""} in
            `Value (id,args_ml,res,k)
          ) idl.cs2ml_arrows
  in

  ~~"namespace %s {\n @[%t@]}\n " ns
    (all [
     ~~"static public partial class %s {\n @[%t@]}\n " !stubclass
       (all [
        all (List.map cs_typstub !Resolution.typedecls);

        print_cs2ml_cs_stubs cs2ml;
        list (fun (_,_,id) -> declare_ml2cs_stub id 1) idl.cs2ml_arrows;
        list closure_class idl.ml2cs_arrows;

        (if cs2ml = [] then empty else
        ~~"public static IntPtr Resolver(string s) { switch (s) {\n @[%t@]}\n return IntPtr.Zero;\n }\n "
          (list
             (function
               | `Value (id, _, _, _) ->
                   ~~"case %S: if (cs2ml_fptr_%s == null) cs2ml_fptr_%s = cs2ml_stub_%s; return Csml.ToIntPtr(cs2ml_fptr_%s);\n " id id id id id
               | _ -> empty
             ) cs2ml));

        ~~"public static void Init() {\n @[%t@]}\n "
          (all [
           ~~"%s.Init();\n " nss;
           (if cs2ml = [] then empty else ~~"Csml.RegisterCS2MLResolver(Resolver);\n ");
         ]);
      ])
   ])

let mlstub_type = function
  | CS2ML_T (_, _, ml, PCs_abstract, _) -> ~~"and %s\n " ml
  | CS2ML_T (_, _, ml, PCs_enum (constrs, false), _) ->
      ~~"and %s = %s\n " ml (String.concat " | " constrs)
  | CS2ML_T (_, _, ml, PCs_enum (constrs, true), _) ->
      ~~"and %s = [ `%s ]\n " ml (String.concat " | `" constrs)
  | CS2ML_T (_, _, _, PCs_mlclass {contents=None; _}, _) -> assert false
  | CS2ML_T (_, _, id, PCs_mlclass {contents=Some cl; _}, _) ->
      let f = function
        | PMLClass_method (_, n, arr, _) -> ~~"%s: %t;\n " n (method_type arr)
        | PMLClass_inline (n, t, _) -> ~~"%s: %s;\n " n t
      in
      let rec dump id (inh, meths) =
        (match inh with
        | None -> empty
        | Some (CS2ML_T (_, _, id, PCs_mlclass {contents=Some cl; _}, _)) ->
            dump id cl
        | _ -> assert false)
          ++ ~~ "is_%s: unit;\n " id
          ++ all (List.map f meths)
      in
      ~~"and %s = < @[cshandle: Csml_iface.cshandle;\n %t@] >\n " id (dump id cl)
  | _ -> empty

let mlfiles idl cs =
  let stub = print_ml2cs_ml_stubs idl cs in
  let first = ref None in
  let opens =
    let rec collect_opens accu l =
      List.fold_left
        (fun accu (_, _, idl) ->
          let accu = collect_opens accu idl.uses in
          match idl.ml with
          | [] -> accu
          | (_, m, _) :: _ -> accu ++ ~~"open %s\n " m
        ) accu l
    in
    ~~"[@@@warning \"-k\"]\n " ++
    collect_opens empty idl.idl.uses
  in
  let pr (s,m,d) =
    all
      [
       begin match !first with
       | None ->
           first := Some m;
           let typedecls = list mlstub_type !Resolution.typedecls in
           opens ++ ~~"type dummy_csml\n " ++ typedecls
       | Some m -> opens ++ ~~"open %s\n " m;
       end;
       print_ml_import d;
       ~~"\n ";
       (if s then stub else empty);
     ]
  in
  List.iter (fun (f,m,d) -> print_to_file f pr (f = idl.idl.mlstub,m,d)) idl.tml;
  if not (List.exists (fun (f, _, _) -> f = idl.idl.mlstub) idl.tml) then
    print_to_file idl.idl.mlstub (fun () ->
      match !first with
      | None -> opens ++ stub
      | Some m -> opens ++ ~~"open %s\n " m ++ stub)
      ()


let csfiles idl ml =
  let stdusing = all [
    ~~"using System;\n ";
    ~~"using System.Runtime.InteropServices;\n ";
    ~~"using System.Collections.Generic;\n "
  ] in
  let stub = print_cs_stub idl ml in
  let import (s,d) = all [
    stdusing;
    print_cs_import d;
    (if s then stub else empty);
  ] in
  List.iter (fun (f,d) -> print_to_file f import (f = idl.idl.csstub,d)) idl.tcs;
  if not (List.mem idl.idl.csstub (List.map fst idl.tcs)) then
    print_to_file idl.idl.csstub (fun () -> stdusing ++ stub) ()


let create_files idl ml cs =
  stubclass := idl.idl.csid;
  mlfiles idl cs;
  csfiles idl ml
