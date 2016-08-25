(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

type ptr
external release_cs_value: 'a -> unit = "release_cs_value"
external resolve_cs2ml: string -> Obj.t = "resolve_cs2ml"

external csml_pop: unit -> 'a = "csml_pop_from_ml"

external cs2ml_apply1:  ptr -> 'a1 -> 'b = "cs2ml_apply1"
external cs2ml_apply2:  ptr -> 'a1 -> 'a2 -> 'b = "cs2ml_apply2"
external cs2ml_apply3:  ptr -> 'a1 -> 'a2 -> 'a3 -> 'b = "cs2ml_apply3"
external cs2ml_apply4:  ptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b = "cs2ml_apply4"
external cs2ml_apply5:  ptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b = "cs2ml_apply5_byte" "cs2ml_apply5"
external cs2ml_apply6:  ptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b = "cs2ml_apply6_byte" "cs2ml_apply6"
external cs2ml_apply7:  ptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b = "cs2ml_apply7_byte" "cs2ml_apply7"
external cs2ml_apply8:  ptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'b = "cs2ml_apply8_byte" "cs2ml_apply8"
external cs2ml_apply9:  ptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'b = "cs2ml_apply9_byte" "cs2ml_apply9"
external cs2ml_apply10: ptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'a10 -> 'b = "cs2ml_apply10_byte" "cs2ml_apply10"
external cs2ml_apply11: ptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'a10 -> 'a11 -> 'b = "cs2ml_apply11_byte" "cs2ml_apply11"
external win32env: unit -> string = "csml_get_win32_environ"

(* Windows has several functions to read/set environment variables.
   - GetEnvironmentVariable/SetEnvironmentVariable access the real environment.
   - getenv reads from a local copy maintained by msvcrt.
   - putenv writes into this local copy and propagate to the real environment.

   OCaml functions Sys.getenv/Unix.putenv uses the getenv/putenv C functions.
   Changes made directly to the real environment (in the C# code) are thus
   invisible from OCaml. The function below refreshes the local copy.
 *)

let refresh_win32_env () =
  let current_env = Hashtbl.create 16 in
  Array.iter
    (fun s ->
      let i = String.index s '=' in
      Hashtbl.replace current_env (String.sub s 0 i) ()
    ) (Unix.environment ());
  let s = win32env () in
  let j = ref 0 in
  let last_eq = ref None in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\000' ->
        begin match !last_eq with
        | Some e ->
            let k = String.sub s !j (e - !j) in
            let v = String.sub s (e + 1) (i - e - 1) in
            if k <> "" then begin
              Hashtbl.remove current_env k;
              Unix.putenv k v
            end
        | None -> ()
        end;
        last_eq := None;
        j := i + 1
    | '=' when !last_eq = None ->
        last_eq := Some i
    | _ -> ()
  done;
  Hashtbl.iter (fun s () -> Unix.putenv s "X"; Unix.putenv s "") current_env

let () =
  match Sys.os_type with
    | "Unix" -> ()
    | _ ->
	try refresh_win32_env ()
	with Unix.Unix_error(error, f, v) ->
	  print_endline (Printf.sprintf "File error: %s [%S] [%S]" (Unix.error_message error) f v)

let () =
  Callback.register_exception "csharp_exception"
    (Csml_iface.Csharp_exception ("","",Obj.magic 0))

let () =
  Csml_iface.release_cs_value := Obj.repr release_cs_value;
  Csml_iface.csml_pop := Obj.repr csml_pop;
  Csml_iface.resolve_cs2ml :=
    (fun n s ->
      let f = resolve_cs2ml s in
      if Obj.magic f = () then raise Not_found;
      let f = Obj.magic f in
      match n with
      | 1 -> Obj.repr (cs2ml_apply1 f)
      | 2 -> Obj.repr (cs2ml_apply2 f)
      | 3 -> Obj.repr (cs2ml_apply3 f)
      | 4 -> Obj.repr (cs2ml_apply4 f)
      | 5 -> Obj.repr (cs2ml_apply5 f)
      | 6 -> Obj.repr (cs2ml_apply6 f)
      | 7 -> Obj.repr (cs2ml_apply7 f)
      | 8 -> Obj.repr (cs2ml_apply8 f)
      | 9 -> Obj.repr (cs2ml_apply9 f)
      | 10 -> Obj.repr (cs2ml_apply10 f)
      | 11 -> Obj.repr (cs2ml_apply11 f)
      | _ -> Printf.eprintf "Wrong arity for register_csml_callback(%s,%n)\n%!" s n; exit 2
    );
  Csml_iface.csharp_available := true
