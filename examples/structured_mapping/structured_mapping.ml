type t =
  | Lit of float
  | Var of string
  | Plus of t list
  | Exp of t * t
  | Frac of frac

and frac = {num: t; den: t}


let rec eval e env =
  match e with
  | Lit f -> f
  | Var x -> List.assoc x env
  | Plus l -> List.fold_left (+.) 0. (List.map (fun x -> eval x env) l)
  | Exp (x, y) -> eval x env ** eval y env
  | Frac f -> eval f.num env /. eval f.den env



(* Other examples *)

type myrecord = {
  x: int;
  y: myrecord option;
}


type myvariant =
  | A
  | B of string * int
  | C of myvariant
