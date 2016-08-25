type context = unit

let counter = ref 0.

let multiply () x y =
  counter := !counter +. 1.;
  x *. y +. !counter

let version () =
  Sys.ocaml_version
