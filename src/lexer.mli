(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

type loc
val fileloc: string -> loc
val printloc: Format.formatter -> loc -> unit

exception Error of loc * string
val error: loc -> string -> 'a

type token = I of string | S of string | Str of string | Lit of string | EOF

val lex: string -> (loc * token) list
val string_of_token: (loc * token) list -> string
