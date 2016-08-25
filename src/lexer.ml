(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

type loc = {
    filename: string;
    text: string;
    pos: int;
  }

let fileloc fn = {filename=fn;text="";pos=(-1)}

type token = I of string | S of string | Str of string | Lit of string | EOF

let string_of_token = function
  | (_, I s) :: _ -> Printf.sprintf "identifier %S" s
  | (_, S s) :: _ -> Printf.sprintf "symbol %S" s
  | (_, Str s) :: _ -> Printf.sprintf "string %S" s
  | (_, Lit s) :: _ -> Printf.sprintf "literal %S" s
  | (_, EOF) :: _
  | [] -> Printf.sprintf "end of input"

let id_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '~' -> true
  | _ -> false

let is_ws = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

exception Error of loc * string

let readfile fn =
  let ic = open_in fn in
  let b = Buffer.create 128 in
  (try while true do Buffer.add_string b (input_line ic); Buffer.add_char b '\n' done
  with End_of_file -> ());
  close_in ic;
  Buffer.contents b

let error loc s = raise (Error (loc, s))

let lex filename =
  let s = readfile filename in
  let len = String.length s in
  let tokens = ref [] in
  let loc i = {filename;text=s;pos=i} in
  let error i txt = error (loc i) txt in
  let has i t =
    i + String.length t <= String.length s && (String.sub s i (String.length t) = t)
  in
  let rec tok j i t =
    tokens := (loc i, t) :: !tokens;
    lex j
  and lex i =
    if i = len then tokens := (loc i, EOF) :: !tokens
    else if id_char s.[i] then lex_id i (i+1)
    else if is_ws s.[i] then lex (i+1)
    else if s.[i] = '\"' then lex_string (i+1) (i+1)
    else if has i "//" then lex_comment_eol (i+2)
    else if has i "(*" then lex_caml_comment 0 (i+2)
    else if has i "/*" then lex_c_comment (i+2)
    else if has i "->" then tok (i+2) i (S "->")
    else if has i "[*" then lex_lit (i+2) (i+2)
    else tok (i+1) i (S (String.sub s i 1))
  and lex_lit i0 i =
    if i = len then error i "Literal not terminated"
    else if has i "*]" then tok (i+2) i0 (Lit (String.sub s i0 (i - i0)))
    else lex_lit i0 (i + 1)
  and lex_id i0 i =
    if i = len || not (id_char s.[i])
    then tok i i0 (I (String.sub s i0 (i-i0)))
    else
      if s.[i] = '.' && i + 1 < len && s.[i+1] = '('
      then lex_id_op i0 (i+2)
      else lex_id i0 (i+1)
  and lex_id_op i0 i =
    if i = len then error i "Identifier not terminated"
    else if s.[i] = ')'
    then tok (i + 1) i0 (I (String.sub s i0 (i - i0 + 1)))
    else lex_id_op i0 (i + 1)
  and lex_comment_eol i =
    if i = len || s.[i] = '\n' then lex i
    else lex_comment_eol (i+1)
  and lex_caml_comment depth i =
    if i = len then error i "Comment not terminated"
    else if has i "*)" then
      if depth > 0 then lex_caml_comment (depth-1) (i+2)
      else lex (i+2)
    else if has i "(*" then lex_caml_comment (depth+1) (i+2)
    else lex_caml_comment depth (i+1)
  and lex_c_comment i =
    if i = len then error i "Comment not terminated"
    else if has i "*/" then lex (i+2)
    else lex_c_comment (i+1)
  and lex_string i0 i =
    if i = len then error i0 "String not terminated"
    else if s.[i] = '\"' then tok (i+1) i0 (Str (String.sub s i0 (i-i0)))
    else lex_string i0 (i+1)
  in
  lex 0;
  List.rev !tokens

let linecol loc =
  let l = ref 1 and last_bol = ref 0 in
  let s = loc.text in
  for j = 0 to loc.pos - 1 do
    if s.[j] = '\n' then (last_bol := j; incr l);
    if s.[j] = '\r' then last_bol := j
  done;
  (!l, max 0 (loc.pos - !last_bol - 1))

let printloc ppf loc =
  if loc.pos = -1 then
    Format.fprintf ppf "File %S" loc.filename
  else
    let (l, c) = linecol loc in
    Format.fprintf ppf "File %S, line %i, char %i" loc.filename l c
