(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Lexer

(* AST *)

type ptyp = PTyp of loc * string * ptyp list | PArrow of arrow
and arrow = ptyp list * ptyp option

(* IDL AST for C# -> ML *)

type cs_kind =
  | Ctor
  | Instance of string
  | Static of string
  | GetProp of string
  | SetProp of string
  | SetItemProp of string
  | GetItemProp of string
  | OpProp of string * string
  | StaticGetProp of string
  | StaticSetProp of string
  | StaticSetItemProp of string
  | StaticGetItemProp of string
  | StaticOpProp of string * string
  | Kill
  | IsNull
  | Null
  | Cast
  | MLCode of string

type cs_bind = {
    cs_kind: cs_kind;
    cs_ignore: bool;
    cs_loc: loc;
  }

type class_component =
  | MLClass_method of string * arrow * cs_bind
  | MLClass_inline of string * string * string

type cs_manifest =
  | Cs_enum of string list * bool
  | Cs_abstract
  | Cs_as_int
  | Cs_mlclass of string option * class_component list

type ml_declaration =
  | Type of loc * string * ptyp * cs_manifest
  | Value of string * arrow * cs_bind
  | Module of string * ml_declaration list
  | MLInline of string

(* IDL AST for ML -> C# *)

type flags = { public: bool; private_: bool; static: bool; override: bool }

type record = (bool * string * string * ptyp) list
type manifest =
  | SumType of bool * (string * string * record) list
  | RecordType of bool * record
  | Abstract

type ml_kind =
  | Fun of string
  | MlKill

type class_declaration =
    flags * string * (ptyp * manifest) option * field list
and cs_declaration =
  | Namespace of string * cs_declaration list
  | Class of class_declaration
  | CSInline of string
  | CSScope of cs_scope * cs_declaration list

and field =
  | NestedClass of class_declaration
  | Method of loc * flags * string * arrow * ml_kind
  | Constructor of loc * flags * string * ptyp list * ml_kind
  | Property of flags * string * ptyp * ml_kind option * ml_kind option
  | CSInlineClass of string
  | CSScopeClass of cs_scope * field list

and cs_scope =
  | Scope_prefix of string
  | Scope_suffix of string

type mlfile = string * string * ml_declaration list
type csfile = string * cs_declaration list
type variantized = {
    var_ml_long: string;
    var_ml_short: string;
    var_cs_short: string;
    var_cs_long: string;
  }

type declaration =
  | ML of mlfile
  | CS of csfile
  | Open of string
  | Use of string * string
  | CSstub of loc * (string * string)
  | MLstub of loc * string
  | Variantized of variantized

type idl = {
    ml: mlfile list;
    cs: csfile list;
    mlstub: string;
    csstub: string;
    csid: string;
    opens: string list;
    uses: (string * string * idl) list;
    variantized: variantized list;
  }
