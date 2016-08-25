(* This module is part of the OCaml side of this sample application.
   Some of its functions are exported to C#, and it also uses some functions
   imported from C#. *)

let () =
  print_endline "OCAML: Now running the initialization of the OCaml code.";
  if Dynlink.is_native then print_endline "OCAML: NATIVE mode."
  else print_endline "OCAML: BYTECODE mode."

let do_something s =
  Printf.printf "OCAML: do_something called with: %s\n%!" s;
  print_endline "OCAML: I will now call bipbip";
  Getting_started_cs.FooBar.bipbip 10 3;
  print_endline "OCAML: I will now return.";
  Printf.sprintf "** %s **" s
