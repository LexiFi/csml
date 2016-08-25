(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

let ns = "LexiFi.Interop"
let clname = "Csml"
let nss = ns ^ "." ^ clname

let trace_file = ref false

let optiter f = function
  | None -> ()
  | Some x -> f x

let optmap f = function
  | None -> None
  | Some x -> Some (f x)

let mapi f l =
  let rec aux i = function
    | [] -> []
    | hd::tl -> f i hd :: (aux (i+1) tl)
  in
  aux 0 l

let search_dirs : (string * string) list ref = ref []

let print_to_file fn f x =
  if !trace_file then print_endline fn;
  Pp.to_file fn ({Pp.indent=0;width=80}, f x)


type 'a physical_ref =
    {
     mutable contents: 'a;
     uid: int
   }

let physical_ref x =
  let r = { contents = x; uid = Oo.id (object end) } in
  Obj.set_tag (Obj.repr r) Obj.object_tag;
  r


let only_annot x =
  let r = { contents = x; uid = 0 } in
  Obj.set_tag (Obj.repr r) Obj.object_tag;
  r

let list_to_string sep f l = String.concat sep (List.map f l)
