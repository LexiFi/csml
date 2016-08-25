(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

let stubclass = ref "###"

open Ast
open Tast
open Misc

let typedecls = ref []
let ml_types = Hashtbl.create 16
let cs_types = Hashtbl.create 16
let arrow_types = Hashtbl.create 16
let arrow_types_done = ref false

let idnames = Hashtbl.create 16

let init () =
  Hashtbl.clear ml_types;
  Hashtbl.clear cs_types;
  Hashtbl.clear arrow_types;
  typedecls := [];
  arrow_types_done := false;
  Hashtbl.clear idnames

let normaname s =
  String.map
    (function
      | '.' | ' ' -> '_'
      | 'A'..'Z' as c -> Char.lowercase_ascii c
      | ('a'..'z' | '0'..'9' | '_') as c -> c
      | _ -> 'x'
    )
    s

let idname s =
  let t = normaname s in
  let t =
    if not (Hashtbl.mem idnames t) then t
    else
      let rec aux i =
        let ti = Printf.sprintf "%s%i" t i in
        if not (Hashtbl.mem idnames ti) then ti
        else aux (i+1)
      in
      aux 0
  in
  Hashtbl.add idnames t ();
  t

let genid_count = ref 0
let genid () =
  incr genid_count;
  idname (Printf.sprintf "__csml__%s_genid%i" !stubclass !genid_count)

let to_force = ref []

let record_lazy t =
  to_force := (fun () -> ignore (Lazy.force t)) :: !to_force;
  physical_ref t

let finish_arrow_types () =
  if not !arrow_types_done then begin
    let l = !to_force in
    to_force := [];
    List.iter (fun f -> f ()) l;
    assert (!to_force = []);
    arrow_types_done := true
  end

let reg_arrow_type t =
  assert (not !arrow_types_done);
  if not (Hashtbl.mem arrow_types t) then
    Hashtbl.replace arrow_types t (genid ())

let rec regarr p = function
  | List t | Option t | Nullable t | Array t -> regarr p t
  | Tuple tl -> List.iter (regarr p) tl
  | Arrow (args,res) ->
      reg_arrow_type (p,args,res);
      List.iter (regarr (not p)) args; optiter (regarr p) res
  | _ -> ()

let find_cs2ml_arrow_type (args, res) =
  finish_arrow_types ();
  try Hashtbl.find arrow_types (true, args, res)
  with Not_found -> assert false

let find_ml2cs_arrow_type (args, res) =
  finish_arrow_types ();
  try Hashtbl.find arrow_types (false, args, res)
  with Not_found -> assert false

let cannot_resolve loc s =
  Lexer.error loc (Printf.sprintf "Cannot resolve type %s" s)

let regtyp topl cs ml t =
  Hashtbl.add ml_types ml t; Hashtbl.add cs_types cs t;
  if topl then typedecls := t :: !typedecls

let rec textual_mltype = function
  | PTyp (_,"*", tl) -> Printf.sprintf "(%s)" (list_to_string " * " textual_mltype tl)
  | PTyp (_,c, [t]) -> Printf.sprintf "%s %s" (textual_mltype t) c
  | PTyp (_,c, []) -> c
  | PTyp (_,c, tl) -> Printf.sprintf "(%s) %s" (list_to_string "," textual_mltype tl) c
  | PArrow (tl, None) -> Printf.sprintf "(%s -> unit)" (list_to_string "->" textual_mltype tl)
  | PArrow (tl, Some t) -> Printf.sprintf "(%s -> %s)" (list_to_string "->" textual_mltype tl) (textual_mltype t)

let rec textual_cstype = function
  | PTyp (_,c, []) -> c
  | PTyp (_,c, tl) -> Printf.sprintf "%s<%s>" c (list_to_string "," textual_cstype tl)
  | PArrow (tl, None) -> Printf.sprintf "ArrowVoid<%s>" (list_to_string "," textual_cstype tl)
  | PArrow (tl, Some t) -> Printf.sprintf "Arrow<%s>" (list_to_string "," textual_cstype (tl@[t]))

let resolve_mltype path =
  let rec aux = function
    | PTyp (_,"list",[t]) -> List (aux t)
    | PTyp (_,"*",tl) -> Tuple (List.map aux tl)
    | PTyp (_,"option",[t]) -> Option (aux t)
    | PTyp (_,"nullable",[t]) -> Nullable (aux t)
    | PTyp (_,"array",[t]) -> Array (aux t)
    | PArrow (args,res) -> Arrow (List.map aux args, optmap aux res)
    | PTyp (_,"int",[]) -> Int
    | PTyp (_,("int64" | "Int64.t"), []) -> Int64
    | PTyp (_,"float",[]) -> Double
    | PTyp (_,"single",[]) -> Single
    | PTyp (_,"string",[]) -> String
    | PTyp (_,"bool",[]) -> Bool
    | PTyp (_,"exn",[]) -> Exn
    | PTyp (_,"blob",[]) -> Blob
    | PTyp (_,"date",[]) -> Date (* LexiFi-specific *)
    | PTyp (_,"variant",[]) -> Variant (* LexiFi-specific *)
    | PTyp (loc,"weak",[t]) -> Weak (only_annot loc, aux t)
    | PTyp (_,"intptr",[]) -> IntPtr
    | PTyp (loc, _, _) as t ->
        let s = textual_mltype t in
        let rec find = function
          | [] -> cannot_resolve loc s
          | pr::l -> try Hashtbl.find ml_types (pr^s) with Not_found -> find l
        in
        find path
  in
  aux

let record =
  List.map (fun (mut, id, csid, t) -> (mut, id, csid,
                                       record_lazy (lazy (
                                                    let t = resolve_mltype [""] t in
                                                    regarr true t;
                                                    regarr false t;
                                                    t
                                                   ))))

let manifest = function
  | Abstract -> PAbstract
  | SumType (priv, l) -> PSumType (priv, List.map (fun (id, cs, args) -> (id, cs, record args)) l)
  | RecordType (priv, l) -> PRecordType (priv, record l)

let register_types toplevel idl =
  let rec cs2ml pr = function
    | Type (_, ml, cs, m) ->
        let m = match m with
        | Cs_enum (l, b) -> PCs_enum (l, b)
        | Cs_abstract -> PCs_abstract
        | Cs_mlclass _ -> PCs_mlclass (physical_ref None)
        | Cs_as_int -> PCs_as_int
        in
        let cs = textual_cstype cs in
        let ml = pr^ml in regtyp toplevel cs ml (CS2ML_T (cs, ml, normaname ml, m, idl.csid))
    | Value _ -> ()
    | Module (s,decls) -> List.iter (cs2ml (pr ^ s ^ ".")) decls
    | MLInline _ -> ()
  and ml2cs pr = function
    | Namespace (s,decls) -> List.iter (ml2cs (pr ^ s ^ ".")) decls
    | CSScope (_, decls) -> List.iter (ml2cs pr) decls
    | Class c -> ml2cs_class pr c
    | CSInline _ -> ()
  and ml2cs_fields pr = function
    | CSScopeClass (_, sub) -> List.iter (ml2cs_fields pr) sub
    | NestedClass c -> ml2cs_class pr c
    | Constructor _ | Method _ | Property _ | CSInlineClass _ -> ()
  and ml2cs_class pr (_,cs,ml,fields) =
    begin match ml with
    | Some (ml, d) ->
        let ml = textual_mltype ml in
        (* TODO: check that the type is not built-in. *)
        let cs = pr^cs in regtyp toplevel cs ml (ML2CS_T (cs, ml, manifest d))
    | None -> ()
    end;
    List.iter (ml2cs_fields (pr ^ cs ^ ".")) fields
  in
  let variantized v =
    regtyp toplevel v.var_cs_short v.var_ml_short (Variantized (v.var_cs_long, v.var_ml_long, idname (v.var_ml_long ^ "_to_variant"), idname (v.var_ml_long ^ "_of_variant"), idl.csid))
  in
  List.iter variantized idl.variantized;
  List.iter (fun (_,m,d) -> List.iter (cs2ml (m ^ ".")) d) idl.ml;
  List.iter (fun (_,d) -> List.iter (ml2cs "") d) idl.cs

let extract_cs2ml idl =
  let rec cs2ml pr path = function
    | Module (n,sub) -> PModule (n, List.map (cs2ml (pr^n^".") (pr::path)) sub)
    | Type (loc,t,cs,m) ->
        let id, this, pm = match arg (pr::path) (PTyp (loc,t,[])) with
        | CS2ML_T (_, _, id, pm, _) as t -> id, t, pm
        | _ -> assert false
        in
        begin match m, pm with
        | Cs_mlclass (inh, meths), PCs_mlclass r ->
            let ppr = pr ^ t ^ "." in
            let f = function
              | MLClass_method (n, arr, k) -> PMLClass_method (idname (ppr ^ n), n, arrow ~this pr path arr, k)
              | MLClass_inline (n, t, impl) -> PMLClass_inline (n, t, impl)
            in
            let inh = match inh with
            | None -> None
            | Some t ->
                match arg (pr::path) (PTyp (loc,t,[])) with
                | CS2ML_T (_, _, _, PCs_mlclass _, _) as t -> Some t
                | _ -> Lexer.error loc "Inherited type is not bound to an ML object type"
            in
            r.contents <- Some (inh, List.map f meths)
        | _ -> ();
        end;
        PType (id, t, pm, textual_cstype cs, pr)
    | Value (n,arr,k) ->
        PValue (idname (pr^n),n, arrow pr path arr,k)
    | MLInline s -> PMLInline s
  and arrow ?this pr path (args, res) =
    let p = arg (pr::path) in
    let args = List.map p args and res = optmap p res in
    List.iter (regarr false) args; optiter (regarr true) res;
    (match this with Some this -> this::args | None -> args), res
  and arg path id = resolve_mltype path id
  in
  List.map (fun (f,m,decls) -> (f,m,List.map (cs2ml (m^".") [""]) decls)) idl.ml

let extract_ml2cs idl =
  let rec ml2cs pr path = function
    | Namespace (n,sub) ->
        PNamespace (n, List.map (ml2cs (pr^n^".") (pr::path)) sub)
    | Class cl -> PClass (ml2cs_class pr path cl)
    | CSInline s -> PCSInline s
    | CSScope (x, sub) -> PCSScope (x, List.map (ml2cs pr path) sub)
  and ml2cs_class pr path (flags,n,w,fields) =
    let w = match w with
    | None -> None
    | Some (id, m) -> Some (textual_mltype id, manifest m) in
    (flags, n, w, List.map (field (pr^n^".") (pr::path)) fields)
  and field pr path = function
    | CSScopeClass (x, sub) -> PCSScopeClass (x, List.map (field pr path) sub)
    | NestedClass cl -> PNestedClass (ml2cs_class pr path cl)
    | Method (loc, flags, n, (args,res), ml) ->
        let p = arg (pr::path) in
        let args = List.map p args and res = optmap p res in
        let args =
          match this_type flags pr with None -> args | Some t -> t :: args in
        List.iter (regarr true) args; optiter (regarr false) res;
        begin
          match ml with
          | MlKill ->
              if flags.static then
                Lexer.error loc "Kill method cannot be static";
              if List.length args <> 1 || res <> None then
                Lexer.error loc "Invalid type for kill method";
          | _ -> ()
        end;
        PMethod (idname (pr ^ n), flags, n, (args, res), ml)
    | Constructor (loc, flags, n, args, ml) ->
        let p = arg (pr::path) in
        let args = List.map p args in
        let res = match p (PTyp (loc,n,[])) with
        | ML2CS_T (fn, _, PAbstract) as t when fn ^ "." = pr -> Some t
              (* TODO: support for constructor for record manifest. *)
        | _ -> Lexer.error loc "The name of the constructor must be the name of its class"
        in
        List.iter (regarr true) args; optiter (regarr false) res;
        PConstructor (idname pr, flags, n, (args, res), ml)
    | Property (flags, n, res, get, set) ->
        let p = arg (pr::path) in
        let res = p res in
        let k s po = optmap (fun ml -> regarr po res; idname (pr^n^s), ml) in
        let get = k "_get" true get in
        let set = k "_set" false set in
        PProperty (flags, n, this_type flags pr, res, get, set)
    | CSInlineClass s ->
        PCSInlineClass s

  and this_type fl pr =
    if fl.static then None
    else
      let cl = String.sub pr 0 (String.length pr - 1) in
      try Some (Hashtbl.find cs_types cl) with Not_found -> assert false

  and arg path =
    let rec aux = function
      | PTyp (_,"List",[t]) -> List (aux t)
      | PTyp (_,"Option",[t]) -> Option (aux t)
      | PTyp (_,"Nullable",[t]) -> Nullable (aux t)
      | PTyp (_,"[]",[PTyp(_,"byte",[])]) -> Blob
      | PTyp (_,"[]",[t]) -> Array (aux t)
      | PTyp (_,"Tuple",tl) -> Tuple (List.map aux tl)
      | PTyp (_,("int"|"Int32"|"System.Int32"),[]) -> Int
      | PTyp (_,("long"|"Int64"|"System.Int64"),[]) -> Int64
      | PTyp (_,("double"|"Double"|"System.Double"),[]) -> Double
      | PTyp (_,("single"|"Single"|"System.Single"),[]) -> Single
      | PTyp (_,("string"|"String"|"System.String"),[]) -> String
      | PTyp (_,("bool"|"Bool"|"System.Bool"),[]) -> Bool
      | PTyp (_,("Exception"|"System.Exception"),[]) -> Exn
      | PTyp (_,("DateTime"|"System.DateTime"),[]) -> Date (* LexiFi-specific *)
      | PTyp (_,"Variant",[]) -> Variant (* LexiFi-specific *)
      | PTyp (loc,"Weak",[t]) -> Weak (only_annot loc, aux t)
      | PTyp (_,"IntPtr",[]) -> IntPtr
      | PTyp (loc, s, []) ->
          let rec find = function
            | [] -> cannot_resolve loc s
            | pr::l ->
                try Hashtbl.find cs_types (pr^s) with Not_found -> find l
          in
          find path
      | PTyp(loc, s, _) ->
          cannot_resolve loc s
      | PArrow (args,res) -> Arrow (List.map aux args, optmap aux res)
    in
    aux
  in
  List.map (fun (f,decls) -> (f,List.map (ml2cs "" []) decls)) idl.cs

let arrow_types_list b =
  finish_arrow_types ();
  Hashtbl.fold (fun (b0,args,res) id accu -> if b = b0 then (args,res,id) :: accu else accu) arrow_types []

let resolve fn =
  init ();
  let rec usefile (_, _, idl) = List.iter usefile idl.uses; register_types false idl; ignore (extract_cs2ml idl) in
  let idl = Parser.parse fn in
  stubclass := idl.csid;
  List.iter usefile idl.uses;
  register_types true idl;
  let ml = extract_cs2ml idl in
  let cs = extract_ml2cs idl in
  { tml = ml;
    tcs = cs;
    cs2ml_arrows = arrow_types_list true;
    ml2cs_arrows = arrow_types_list false;
    idl }
