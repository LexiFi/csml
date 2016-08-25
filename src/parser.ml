(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Ast
open Lexer
open Misc

let modname f =
  let m = Filename.basename f in
  let m = Filename.chop_extension m in
  String.capitalize_ascii m

let find_file fn =
  if Sys.file_exists fn
  then (fn, fn)
  else
    let rec aux = function
      | [] -> raise Not_found
      | (real, ext) :: tl ->
          let s = Filename.concat real fn in
          if Sys.file_exists s then (s, Filename.concat ext fn)
          else aux tl
    in
    aux !search_dirs

let error s rest =
  let loc =
    match rest with
    | (loc, _) :: _ -> loc
    | [] -> assert false
  in
  let s = Printf.sprintf "%s. Next token: %s" s (Lexer.string_of_token rest) in
  Lexer.error loc s

let opt f rest = try Some (f rest) with Error _ -> None

let getloc = function
  | (loc,_)::_ -> loc
  | [] -> assert false

let many f =
  let rec aux accu rest =
    match f rest with
    | None -> List.rev accu, rest
    | Some (d, rest) -> aux (d::accu) rest
  in
  aux []

let plus_sep sep f =
  let rec aux accu rest =
    let x, rest = f rest in
    let accu = x :: accu in
    match rest with
    | (_, S s) :: rest when s = sep -> aux accu rest
    | rest -> List.rev accu, rest
  in
  aux []

let expect_s e = function
  | (_, S s) :: rest when s = e -> rest
  | rest -> error (e ^ " expected") rest
let skip_opt_s e = function
  | (_, S s) :: rest when s = e -> rest
  | rest -> rest
let expect_i e = function
  | (_, I s) :: rest when s = e -> rest
  | rest -> error (e ^ " expected") rest
let id = function
  | (_, I s) :: rest -> s, rest
  | rest -> error "identifier expected" rest
let idloc = function
  | (loc, I s) :: rest -> (s, loc), rest
  | rest -> error "identifier expected" rest

let get_str = function
  | (_, Str s) :: rest -> s, rest
  | rest -> error "string expected" rest

let has tok = function
  | (_, hd) :: rest when hd = tok -> true, rest
  | rest -> false, rest

let get_lit = function
  | (_, Lit s) :: rest -> s, rest
  | rest -> error "literal expected" rest

let get_id_or_lit = function
  | (_, (I s | Lit s)) :: rest -> s, rest
  | rest -> error "identifier or literal expected" rest

let rec declarations rest =
  let d, _ = many declaration rest in
  d

and declaration = function
  | (_, EOF) :: _ -> None
  | (loc, I id) :: rest as rest0 -> Some (declaration_aux loc rest0 rest id)
  | rest -> error "top-level declaration" rest
and declaration_aux loc rest0 rest = function
  | "mlfile" ->
      let f, rest = get_str rest in
      let d, rest = many ml_declaration rest in
      ML (f, modname f, d),  rest
  | "csfile" ->
      let f, rest = get_str rest in
      let d, rest = many cs_declaration rest in
      CS (f, d), rest
  | "mlstub" ->
      let f, rest = get_str rest in
      MLstub (loc, f), rest
  | "csstub" ->
      let f, rest = get_str rest in
      let id, rest = id rest in
      CSstub (loc, (f, id)), rest
  | "open" ->
      let id, rest = id rest in
      Open id, rest
  | "use" ->
      let f, rest = get_str rest in
      let f, ext =
        try find_file f
        with Not_found -> error ("cannot find file " ^ f) rest0
      in
      Use (f, ext), rest

(* BEGIN LexiFi-specific part *)
  | "variantized" ->
      let t, rest = mltype rest in
      let rest = expect_s ":" rest in
      let ml_short, rest = id rest in
      let cs_short, rest = id rest in

      let rec cs = function
        | PTyp (_,"list",[t]) -> Printf.sprintf "%s_list" (cs t)
        | PTyp (_,"*",tl) -> "tuple_" ^ list_to_string "_" cs tl ^ "_end"
        | PTyp (_,"option",[t]) -> Printf.sprintf "%s_option" (cs t)
        | PTyp (_,"variant", []) -> "Variant"
        | PTyp (_,"float", []) -> "double"
        | PTyp (_,"date", []) -> "DateTime"
        | PTyp (_,c, tl) ->
            let c = String.map (function '.' -> '_' | c -> c) c in
            list_to_string "_" cs tl ^ (if tl = [] then "" else "_") ^ c
        | _ -> assert false
      in
      let rec ml = function
        | PTyp (_,"list",[t]) -> Printf.sprintf "%s list" (ml t)
        | PTyp (_,"*",tl) -> "(" ^ list_to_string "*" ml tl ^ ")"
        | PTyp (_,"option",[t]) -> Printf.sprintf "%s option" (ml t)
        | PTyp (_,"variant", []) -> "Mlfi_isdatypes.variant"
        | PTyp (_,c, []) -> c
        | PTyp (_,c, tl) ->
            "(" ^ list_to_string "," ml tl ^ ") " ^ c
        | _ -> assert false
      in
      Variantized {var_ml_short=ml_short;
                   var_ml_long=ml t;
                   var_cs_short=cs_short;
                   var_cs_long="LexiFi.Apropos.Generated." ^ (cs t)},
      rest
(* END LexiFi-specific part *)

  | _ -> error "top-level declaration" rest0

and cs_scope = function
  | (_, I "prefix") :: rest ->
      let s, rest = get_lit rest in
      Scope_prefix s, rest
  | (_, I "suffix") :: rest ->
      let s, rest = get_lit rest in
      Scope_suffix s, rest
  | rest -> error "in using scope" rest

and cs_declaration = function
  | (_, I "namespace") :: rest -> Some (parse_namespace rest)
  | (_, I "inline") :: rest -> let s, rest = get_lit rest in Some (CSInline s, rest)
  | (_, I "using") :: rest ->
      let x, rest = cs_scope rest in
      let sub, rest = cs_declarations rest in
      Some (CSScope (x, sub), rest)
  | rest ->
      let flags, rest =
        parse_flags {public=false;private_=false;static=false;override=false} rest in
      let cl, rest = has (I "class") rest in
      if not cl then None
      else
        let c, rest = parse_class flags rest in
        Some (Class c, rest)

and parse_namespace = function
  | (_, I name) :: rest ->
      let sub, rest = cs_declarations rest in
      Namespace (name,sub), rest
  | rest -> error "in namespace" rest

and cs_declarations rest =
  let rest = expect_s "{" rest in
  let sub, rest = many cs_declaration rest in
  let rest = expect_s "}" rest in
  (sub, rest)

and parse_id_as rest =
  let id, rest = id rest in
  match rest with
  | (_, I "as") :: (_, I cs) :: rest -> id, cs, rest
  | _ -> id, id, rest

and parse_record rest =
  let rec fields l = function
    | (_, S "}") :: rest -> List.rev l, rest
    | rest ->
        let mut, rest = has (I "mutable") rest in
        let id, csname, rest = parse_id_as rest in
        let rest = expect_s ":" rest in
        let t, rest = mltype rest in
        let l = (mut, id, csname, t) :: l in
        match rest with
        | (_, S ";") :: rest -> fields l rest
        | (_, S "}") :: rest -> List.rev l, rest
        | _ -> error "} or ; expected" rest
  in
  let rest = expect_s "{" rest in
  let fields, rest = fields [] rest in
  fields, rest

and parse_manifest_record priv rest =
  let fields, rest = parse_record rest in
  RecordType (priv, fields), rest

and parse_manifest_sum priv rest =
  let constr rest =
    let id, csname, rest = parse_id_as rest in
    let fields, rest =
      match rest with
      | (_, I "of") :: ((_, S "{") :: _ as rest) ->
          parse_record rest
      | (_, I "of") :: rest ->
          let tl, rest = plus_sep "*" mltype2 rest in
          mapi (fun i t -> true, "", Printf.sprintf "TVal%i" i, t) tl, rest
      | rest ->
          [], rest
    in
    (id, csname, fields), rest
  in
  let rest = skip_opt_s "|" rest in
  let constrs, rest = plus_sep "|" constr rest in
  SumType (priv, constrs), rest


and parse_manifest = function
  | (_, S "=") :: rest ->
      let priv, rest = has (I "private") rest in
      begin match rest with
      | (_, S "{") :: _ -> parse_manifest_record priv rest
      | _ -> parse_manifest_sum priv rest
      end
  | rest -> Abstract, rest

and parse_wraps = function
  | (_, S "=") :: rest ->
      let t, rest = mltype rest in
      let m, rest = parse_manifest rest in
      Some (t, m), rest
  | rest -> None, rest

and parse_class flags = function
  | (_, I name) :: rest ->
      let wraps, rest = parse_wraps rest in
      let fields, rest = cs_fields rest in
      (flags, name, wraps, fields), rest
  | rest -> error "in class" rest

and cs_fields rest =
  let rest = expect_s "{" rest in
  let fields, rest = many field rest in
  let rest = expect_s "}" rest in
  fields, rest

and ml_kind = function
  | (_, I "kill") :: rest ->
      MlKill, rest
  | rest ->
      let i, rest = get_id_or_lit rest in
      Fun i, rest

and field = function
  | (_, I "inline") :: rest -> let s, rest = get_lit rest in Some (CSInlineClass s, rest)
  | (_, I "using") :: rest ->
      let x, rest = cs_scope rest in
      let sub, rest = cs_fields rest in
      Some (CSScopeClass (x, sub), rest)
  | rest -> real_field rest

and real_field rest =
  let flags, rest =
    parse_flags {public=false;private_=false;override=false;static=false} rest in
  match rest with
  | (_, I "class") :: rest ->
      let c, rest = parse_class flags rest in
      Some (NestedClass c, rest)
  | (loc, I name) :: (_, S "(") :: rest ->
      (* TODO: check that name is the name of the current class *)
      let args, rest = args rest in
      let rest = expect_s "=" rest in
      let ml, rest = ml_kind rest in
      let rest = expect_s ";" rest in
      Some (Constructor (loc, {flags with static=true}, name, args, ml), rest)
  | (_, I _) :: _ as rest ->
      let res, rest = cstype rest in
      let name, rest = cs_name rest in
      begin match rest with
      | (_, S "(") :: rest ->
          let res = match res with PTyp (_,"void",[]) -> None | t -> Some t in
          let args, rest = args rest in
          let rest = expect_s "=" rest in
          let loc = getloc rest in
          let ml, rest = ml_kind rest in
          let rest = expect_s ";" rest in
          Some (Method (loc, flags, name, (args, res), ml), rest)
      | (_, S "{") :: rest ->
          let f kw = function
            | (_, I k) :: rest when k = kw ->
                let rest = expect_s "=" rest in
                let ml, rest = ml_kind rest in
                let rest = expect_s ";" rest in
                Some ml, rest
            | rest -> None, rest
          in
          let get, rest = f "get" rest in
          let set, rest = f "set" rest in
          let rest = expect_s "}" rest in
          Some (Property (flags, name, res, get, set), rest)
      | rest -> error "( or { expected" rest
      end
  | _ -> None

and cstype_or_name no_array no_arrow_check rest =
  let loc = getloc rest in
  let id, rest = id rest in
  let args, rest =
    match rest with
    | (_, S "<") :: rest ->
        let tl, rest = plus_sep "," cstype rest in
        let rest = expect_s ">" rest in
        tl, rest
    | _ ->
        [], rest
  in
  let t =
    match id with
    | "Arrow" when not no_arrow_check ->
        if args = [] then error "Arrow need at least one type argument" rest;
        let tl = List.rev args in
        PArrow (List.rev (List.tl tl), Some (List.hd tl))
    | "ArrowVoid" when not no_arrow_check -> PArrow (args, None)
    | id -> PTyp (loc, id, args)
  in
  let rec loop_arrays t = function
    | (_, S "[") :: (_, S "]") :: rest -> loop_arrays (PTyp (loc, "[]", [t])) rest
    | rest -> t, rest
  in
  if no_array then
    t, rest
  else
    loop_arrays t rest

and cs_name = function
  | (_, I "operator") :: (_, (S x | I x)) :: rest ->
      Printf.sprintf "operator %s" x, rest
  | rest ->
      let rec str_type = function
          | PTyp (_, name, []) -> name
          | PTyp (_, name, tl) ->
            Printf.sprintf "%s<%s>" name (String.concat ", " (List.map str_type tl))
          | PArrow _ -> assert false
      in
      let typ, rest = cstype_or_name true true rest in
      str_type typ, rest

and args = function
  | (_, S ")") :: rest -> [], rest
  | rest ->
      let tl, rest = plus_sep "," cstype rest in
      let rest = expect_s ")" rest in
      tl, rest

and cstype rest = cstype_or_name false false rest

and parse_flags flags = function
  | (_, I fl) :: rest as rest0 ->
      begin match fl with
      | "public" -> parse_flags { flags with public=true} rest
      | "private" -> parse_flags { flags with private_=true} rest
      | "static" -> parse_flags { flags with static=true} rest
      | "override" -> parse_flags { flags with override=true} rest
      | _ -> flags, rest0
      end
  | rest -> flags, rest

and ml_declaration = function
  | (_, I "module") :: rest -> Some (parse_module rest)
  | (_, I "type") :: rest -> Some (parse_type rest)
  | (_, I "val") :: rest -> Some (parse_val rest)
  | (_, I "class") :: rest -> Some (parse_mlclass rest)
  | (_, I "inline") :: rest -> let s, rest = get_lit rest in Some (MLInline s, rest)
  | _ -> None

and parse_mlclass rest =
  let loc = getloc rest in
  let name, rest = id rest in
  let rest = expect_s "=" rest in
  let t, rest = cstype rest in
  let rest = expect_s ":" rest in
  let rest = expect_i "object" rest in
  let inh, rest = match rest with
  | (_, I "inherit") :: rest ->
      let i,rest = id rest in
      Some i, rest
  | _ -> None, rest in
  let meths, rest = many ml_method rest in
  let rest = expect_i "end" rest in
  Type (loc, name, t, Cs_mlclass (inh, meths)), rest

and ml_method = function
  | (_, I "method") :: rest ->
      let name, rest = id rest in
      let rest = expect_s ":" rest in
      begin match rest with
      | (_, Lit t) :: (_, S "=") :: (_, Lit impl) :: rest ->
          Some (MLClass_inline (name, t, impl), rest)
      | _ ->
          let typ, rest = mltype rest in
          let arr = match typ with
          | PArrow arr -> arr
          | PTyp (_,"unit",[]) -> ([], None)
          | t -> ([], Some t)
          in
          let csbind, rest = parse_csbind rest in
          Some (MLClass_method (name, arr, csbind), rest)
      end
  | _ -> None

and parse_module = function
  | (_, I name) :: (_, S ":") :: (_, I "sig") :: rest ->
      let sub, rest = many ml_declaration rest in
      let rest = expect_i "end" rest in
      Module (name,sub), rest
  | rest -> error "in module" rest

and parse_type rest =
  let name, rest = id rest in
  let rest = expect_s "=" rest in
  let loc = getloc rest in
  let t, rest = cstype rest in
  let m, rest = parse_cs_manifest rest in
  Type (loc, name, t, m), rest

and backquote_id = function
  | (_, S "`") :: (_, I x) :: rest -> x, rest
  | rest -> error "`id expected" rest

and parse_cs_manifest = function
  | (_, S "=") :: rest ->
      let poly, rest = has (S "[") rest in
      let rest = skip_opt_s "|" rest in
      let constrs, rest = plus_sep "|" (if poly then backquote_id else id) rest in
      let rest = if poly then expect_s "]" rest else rest in
      Cs_enum (constrs, poly), rest
  | (_, I "as") :: (_, I "int") :: rest -> Cs_as_int, rest
  | rest -> Cs_abstract, rest

and mltype rest =
  let tl, rest = plus_sep "->" mltype1 rest in
  let t =
    match tl with
    | [] -> assert false
    | [t] -> t
    | tl ->
        let tl = List.rev tl in
        let res = List.hd tl and args = List.rev (List.tl tl) in
        let args = match args with [PTyp (_,"unit",[])] -> [] | l -> l in
        let res = match res with PTyp (_,"unit",[]) -> None | t -> Some t in
        PArrow (args,res)
  in
  t, rest

and mltype1 rest =
  let loc = getloc rest in
  let tl, rest = plus_sep "*" mltype2 rest in
  let t = match tl with [t] -> t | tl -> PTyp (loc,"*", tl) in
  t, rest

and mltype2 rest =
  let tl, rest = match rest with
  | (_, S "(") :: rest ->
      let tl, rest = plus_sep "," mltype rest in
      let rest = expect_s ")" rest in
      tl, rest
  | rest -> [], rest
  in
  let ids, rest = many (opt idloc) rest in
  let tl = List.fold_left (fun tl (id, loc) -> [PTyp (loc, id, tl)]) tl ids in
  match tl with
  | [t] -> t, rest
  | _ -> error "type constructor expected" rest

and parse_csbind rest =
  let rest = expect_s "=" rest in
  let loc = getloc rest in
  let ign, rest = has (I "ignore") rest in
  let kind, rest =
    match rest with
    | (_, I "ctor") :: rest -> Ctor, rest
    | (_, I "instance") :: (_, I v) :: rest -> Instance v, rest
    | (_, I "static") :: (_, I "get") :: (_, I v) :: rest -> StaticGetProp v, rest
    | (_, I "static") :: (_, I "set") :: (_, I "op") :: rest -> let v, rest = id rest in let op, rest = get_str rest in StaticOpProp (v, op), rest
    | (_, I "static") :: (_, I "set") :: (_, I v) :: rest -> StaticSetProp v, rest
    | (_, I "static") :: (_, I "indexed") :: (_, I "set") :: (_, I v) :: rest -> StaticSetItemProp v, rest
    | (_, I "static") :: (_, I "indexed") :: (_, I "get") :: (_, I v) :: rest -> StaticGetItemProp v, rest
    | (_, I "static") :: rest -> let name, rest = cs_name rest in Static name, rest
    | (_, I "indexed") :: (_, I "set") :: (_, I v) :: rest -> SetItemProp v, rest
    | (_, I "indexed") :: (_, I "get") :: (_, I v) :: rest -> GetItemProp v, rest
    | (_, I "set") :: (_, I "op") :: rest -> let v, rest = id rest in let op, rest = get_str rest in OpProp (v, op), rest
    | (_, I "set") :: (_, I v) :: rest -> SetProp v, rest
    | (_, I "get") :: (_, I v) :: rest -> GetProp v, rest
    | (_, I "kill") :: rest -> Kill, rest
    | (_, I "isnull") :: rest -> IsNull, rest
    | (_, I "null") :: rest -> Null, rest
    | (_, I "cast") :: rest -> Cast, rest
    | (_, Lit s) :: rest -> MLCode s, rest
    | rest -> error "in kind" rest
  in
  {cs_kind=kind;cs_ignore=ign;cs_loc=loc}, rest

and parse_val = function
  | (_, I name) :: (_, S ":") :: rest ->
      let typ, rest = mltype rest in
      let arr = match typ with
      | PArrow arr -> arr
      | _ -> error "cannot bind non-functional values" rest
      in
      let csbind, rest = parse_csbind rest in
      Value (name, arr, csbind), rest
  | rest -> error "in type" rest

let rec parse fn =
  let err s = Lexer.error (fileloc fn) s in
  let idl = declarations (Lexer.lex fn) in
  let ml = ref []
  and cs = ref []
  and mlstub = ref None
  and csstub = ref None
  and opens = ref []
  and uses = ref []
  and vs = ref []
  in
  List.iter
    (function
      | ML d -> ml := d :: !ml
      | CS d -> cs := d :: !cs
      | Open i -> opens := i :: !opens
      | Use (s, ext) -> uses := (s, ext) :: !uses
      | MLstub (loc, s) ->
          if !mlstub = None then mlstub := Some s
          else Lexer.error loc "Multiple mlstub declaration"
      | CSstub (loc, s) ->
          if !csstub = None then csstub := Some s
          else Lexer.error loc "Multiple csstub declaration"
      | Variantized v ->
          vs := v :: !vs
    ) idl;
  let mlstub = match !mlstub with None -> err "No mlstub declaration" | Some s -> s
  and (csstub, csid) = match !csstub with None -> err "No csstub declaration" | Some s -> s
  in
  {
   ml = List.rev !ml;
   cs = List.rev !cs;
   opens = List.rev !opens;
   uses = List.rev_map (fun (fn, ext) -> fn, ext, parse fn) !uses;
   mlstub;
   csstub;
   csid;
   variantized = !vs;
 }
