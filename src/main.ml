(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

let version = "0.1"

open Misc
open Stub
open Ast

include (Tast : sig end)
include (Pp : sig end)

let gen fn = print_to_file fn Gen_csml_iface.create ()

let dllbind dll =
  Printf.printf "namespace LexiFi.Interop { public partial class Csml { public const string dllname = %S; } }" dll

let mode = ref `Gen

let add_dir s =
  try
    let i = String.index s ',' in
    let realdir = String.sub s 0 i
    and extdir = String.sub s (i + 1) (String.length s - i - 1) in
    search_dirs := (realdir, extdir) :: !search_dirs
  with Not_found ->
    search_dirs := (s, s) :: !search_dirs

let specs = [
  "-gen", Arg.String gen, "<outfile> (internal use)";
  "-dep", Arg.Unit (fun () -> mode := `Dep), " output dependency information";
  "-om", Arg.Unit (fun () -> mode := `Om), " output build information";
  "-dllbind", Arg.String dllbind, " create the C# code to bind to a specific dll";
  "-trace", Arg.Set Misc.trace_file, " display name of created files";
  "-I", Arg.String add_dir, "<dir> Add a search directory for use directives";
]

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

let dep idl =
  let d = ref (StringSet.add idl.csstub (StringSet.add idl.mlstub StringSet.empty)) in
  List.iter (fun (f,_,_) -> d := StringSet.add f !d) idl.ml;
  List.iter (fun (f,_) -> d := StringSet.add f !d) idl.cs;
  StringSet.elements !d

let uses idl =
  let d = ref StringSet.empty in
  let rec aux (_, fn, idl) = d := StringSet.add fn !d; List.iter aux idl.uses in
  List.iter aux idl.uses;
  StringSet.elements !d

let banner =
  Printf.sprintf
    "LexiFi CSML compiler\nVersion %s.\n"
    version

let main () =
  let fns = ref [] in
  Arg.parse (Arg.align specs)
    (fun s -> fns := s :: !fns)
    banner;
  List.iter
    (fun fn ->
      match !mode with
      | `Gen ->
          let tidl = Resolution.resolve fn in
          let ml = cs2ml_stubs tidl in
          let cs = ml2cs_stubs tidl in
          create_files tidl ml cs
      | `Dep ->
          let idl = Parser.parse fn in
          Printf.printf "%s: %s %s\n" (String.concat " " (dep idl)) fn (String.concat " " (uses idl))
      | `Om ->
          let idl = Parser.parse fn in
          let fn = String.map (function '\\' -> '/' | c -> c) fn in
          Printf.printf "CSML(%s, %s, %s, %s, %s)\n"
            fn
            idl.mlstub
            idl.csstub
            (String.concat " " (dep idl))
            (match uses idl with [] -> "$(EMPTY)" | l -> (String.concat " " l))
    )
    !fns


let () =
  try main ()
  with Lexer.Error (loc, msg) ->
    Format.eprintf "%a@.Error: %s@." Lexer.printloc loc msg;
    exit 1
