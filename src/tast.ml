(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

type cs_bind = Ast.cs_bind
type flags = Ast.flags

type typ =
  | Int
  | Int64
  | Double
  | Single
  | Date
  | String
  | Bool
  | ML2CS_T of string * string * pmanifest
  | CS2ML_T of string * string * string * pcs_manifest * string
  | List of typ
  | Array of typ
  | Tuple of typ list
  | Option of typ
  | Nullable of typ
  | Arrow of typ list * typ option
  | Exn
  | Blob
  | IntPtr
  | Variant
  | Variantized of string * string * string * string * string
  | Weak of Lexer.loc Misc.physical_ref * typ

and pmanifest =
  | PSumType of bool * (string * string * precord) list
  | PRecordType of bool * precord
  | PAbstract

and precord = (bool * string * string * typ Lazy.t Misc.physical_ref) list

and parrow = typ list * typ option

and pclass_component =
  | PMLClass_method of string * string * parrow * cs_bind
  | PMLClass_inline of string * string * string

and pcs_manifest =
  | PCs_enum of string list * bool
  | PCs_abstract
  | PCs_mlclass of (typ option * pclass_component list) option Misc.physical_ref
  | PCs_as_int

type mldecl =
  | PModule of string * mldecl list
  | PType of string * string * pcs_manifest * string * string
  | PValue of (string * string * parrow * cs_bind)
  | PMLInline of string

type csdecl =
  | PNamespace of string * csdecl list
  | PClass of csclass
  | PCSInline of string
  | PCSScope of Ast.cs_scope * csdecl list

and csclass = flags * string * (string * pmanifest) option * csfield list
and csfield =
  | PNestedClass of csclass
  | PMethod of string * flags * string * parrow * Ast.ml_kind
  | PConstructor of string * flags * string * parrow * Ast.ml_kind
  | PProperty of flags * string * typ option * typ *
        (string * Ast.ml_kind) option * (string * Ast.ml_kind) option
  | PCSInlineClass of string
  | PCSScopeClass of Ast.cs_scope * csfield list


type tidl = {
    tml: (string * string * mldecl list) list;
    tcs: (string * csdecl list) list;
    cs2ml_arrows: (typ list * typ option * string) list;
    ml2cs_arrows: (typ list * typ option * string) list;
    idl: Ast.idl;
  }

open Pp.Operators

let comma = Pp.list ~sep:~~","

let rec dump_type = function
  | Int -> ~~"Int"
  | Int64 -> ~~"Int64"
  | Double -> ~~"Double"
  | Single -> ~~"Single"
  | String -> ~~"String"
  | Bool -> ~~"Bool"
  | Date -> ~~"Date"
  | ML2CS_T (cs,ml,_) -> ~~"(cs:%s <- ml:%s)" cs ml
  | CS2ML_T (cs,ml,_,_,_) -> ~~"(cs:%s -> ml:%s)" cs ml
  | IntPtr -> ~~"IntPtr"
  | List t -> ~~"List(%t)" (dump_type t)
  | Array t -> ~~"Array(%t)" (dump_type t)
  | Tuple tl -> ~~"Tuple(%t)" (comma dump_type tl)
  | Option t -> ~~"Option(%t)" (dump_type t)
  | Nullable t -> ~~"Nullable(%t)" (dump_type t)
  | Arrow (tl,None) -> ~~"Arrow(%t -> unit)" (comma dump_type tl)
  | Arrow (tl,Some t) -> ~~"Arrow(%t -> %t)" (comma dump_type tl) (dump_type t)
  | Exn -> ~~"Exn"
  | Blob -> ~~"Blob"
  | Variant -> ~~"Variant"
  | Variantized (cs,ml,_,_,_) -> ~~"Variantized(cs:%s <-> ml:%s)" cs ml
  | Weak (_, t) -> ~~"Weak(%t)" (dump_type t)

let string_of_type t =
  let open Pp in
  to_string ({width=1000; indent=0}, dump_type t)
