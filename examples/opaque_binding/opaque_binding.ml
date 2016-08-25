(* This is an example of an ML type that will be reflected opaquely in C#.
   This means that the C# code only manipulates pointers to values of this type.
   The values themselves never cross the boundary between the two heaps.

   In C#, the pointers to the ML value are wrapped in a class that can define
   more components.
 *)

type t = {
    mutable foo: int;
    mutable bar: int;
  }

(* We define a number of functions to be bound to C# components of the class that wraps
   the type t. *)

let to_string c =
  Printf.sprintf "foo = %i, bar = %i" c.foo c.bar

let get_foo c =
  print_endline "(ML get_foo)";
  c.foo

let set_foo c x =
  print_endline "(ML set_foo)";
  c.foo <- x

let get_bar c =
  print_endline "(ML get_bar)";
  c.bar

let set_bar c x =
  print_endline "(ML set_bar)";
  c.bar <- x

let create foo bar =
  print_endline "(ML create)";
  { foo; bar }

let version () = "1.0"

let test () =
  let c = Opaque_binding_cs.create_init 3 in
  Opaque_binding_cs.bump c;
  Printf.printf "-> %i\n%!" (Opaque_binding_cs.get c)
