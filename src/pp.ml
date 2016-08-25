(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

type pp_info =
  {
    width: int; (** The maximum width of the output for linear mode. *)
    indent: int; (** The initial indentation. *)
  }

let rec next_newline s i =
  if i = String.length s then i
  else match s.[i] with
  | '\r' | '\n' -> i
  | _ -> next_newline s (i + 1)

let rec next_nonws s i =
  if i = String.length s then i
  else match s.[i] with
  | ' ' -> next_nonws s (i + 1)
  | _ -> i

module Printer : sig
  type t
  type f = t -> unit

  val create: pp_info -> (string -> unit) -> t

  val str: string -> f (** the string should not contain \n,\r,\t *)

  val conc: f -> f -> f
  val empty: f
  val indent: int -> f -> f
  val indent_relative: int -> f -> f
  val xloc: f

  val newline: f
  val force_newline: f

  val ifroom: int -> f -> f -> f
  (** [if_before n x y]:
      if the current position on the line is less than [n],
      then print [x], otherwise print [y]. *)

end
=
struct
  type t =
    {
      print: string -> unit;
      pr_width: int;
      mutable x: int; (* current position / beginning of line *)
      mutable ind: int; (* current indentation level *)
    }

  type f = t -> unit

  let white = String.make 8 ' '

  let spaces pr n =
    let l = String.length white in
    for _i = 1 to n / l do pr.print white done;
    for _i = 1 to n mod l do pr.print " " done

  let create i pr =
    {
      print = pr;
      x = 0;
      ind = i.indent;
      pr_width = i.width;
    }

  let force_newline pr = pr.print "\n"; pr.x <- 0
  let newline pr = if pr.x > 0 then force_newline pr

  let empty _ = ()

  let str0 s pr =
    if pr.x = 0 && pr.ind > 0 then (spaces pr pr.ind; pr.x <- pr.ind);
    pr.print s; pr.x <- pr.x + String.length s

  let str s pr =
    let l = String.length s in
    let rec aux i =
      let j = next_newline s i in
      if j = l then begin
        if i < l then str0 (String.sub s i (l - i)) pr
      end
      else begin
        if i < j then str0 (String.sub s i (j - i)) pr;
        (match s.[j] with
        | '\n' -> newline pr | '\r' -> force_newline pr | _ -> assert false);
        aux (next_nonws s (j + 1))
      end
    in
    aux 0

  let xloc pr = str0 (Printf.sprintf "%05i" pr.x) pr

  let conc x y pr = x pr; y pr

  let set_indent n x pr =
    let old = pr.ind in pr.ind <- max 0 n; x pr; pr.ind <- old

  let indent n x pr = set_indent (pr.ind + n) x pr

  let indent_relative n x pr = set_indent (pr.x + n) x pr

  let ifroom n x y pr =
    if pr.x + n < pr.pr_width then x pr
    else y pr
end


module P = Printer

module L = struct
  type t = int * P.f

  let empty = 0, P.empty

  let str s =
    let l = String.length s in
    let b = Buffer.create l in
    String.iter (function '\n' | '\r' -> () | c -> Buffer.add_char b c) s;
    let s = Buffer.contents b in
    String.length s, P.str s

  let conc ((wx, sx) as x) ((wy, sy) as y) =
    if wx = 0 then y
    else if wy = 0 then x
    else (wx + wy, P.conc sx sy)
end

type t = {lin: L.t option; multi: P.f}

type f = unit -> t

let linear_length x =
  match (x ()).lin with Some (w,_) -> w | _ -> max_int

let get x = (x ()).multi
let mk ?lin m () = { lin; multi = m }
let empty = mk ~lin:L.empty P.empty
let str s = mk ~lin:(L.str s) (P.str s)

let conc x y =
  let x = x () and y = y () in
  let lin =
    match x.lin, y.lin with
    | Some x, Some y -> Some (L.conc x y)
    | _ -> None
  in
  mk ?lin (P.conc x.multi y.multi)

let all = List.fold_left conc empty

let xloc = mk ~lin:(5, P.xloc) P.xloc

let force_multiline = mk P.empty

let force_linear x =
  let x' = x () in
  match x'.lin with
  | Some (_,p) as lin -> mk ?lin p
  | None -> x

let newline = mk P.newline
let force_newline = mk P.force_newline
let br = mk ~lin:L.empty P.newline
let either lin multi = mk ?lin:(lin ()).lin (multi ()).multi

let indent n x = let x' = x () in mk ?lin:x'.lin (P.indent n x'.multi)

let indent_relative n x =
  let x' = x () in mk ?lin:x'.lin (P.indent_relative n x'.multi)

let line x =
  let x' = x () in
  match x'.lin with
  | None -> x
  | Some (w,l) as lin -> mk ?lin (P.ifroom w l x'.multi)

let ws = either (str " ") newline

let app f x = str (f x)

let int = app string_of_int
let float = app string_of_float

let list ?sep f l =
  match sep with
  | None -> all (List.map f l)
  | Some sep ->
      let rec aux = function
        | [] -> [ ]
        | [x] -> [ f x ]
        | x::l -> f x :: sep :: aux l
      in
      all (aux l)

let listi ?sep f l =
  let sep = match sep with
    | None -> fun x l -> x :: l
    | Some sep -> fun x l -> x :: sep :: l
  in
  let rec aux i = function
    | [] -> [ ]
    | [x] -> [ f i x ]
    | x::l -> sep (f i x) (aux (i + 1) l)
  in
  all (aux 0 l)

let seq ?sep f i j =
  let sep = match sep with
    | None -> fun x l -> x :: l
    | Some sep -> fun x l -> x :: sep :: l
  in
  let rec aux i =
    if i >= j then []
    else if i + 1 = j then [ f i ]
    else sep (f i) (aux (i + 1))
  in
  all (aux i)

let opt f  = function
  | None -> empty
  | Some x -> f x

type pp_fragment = pp_info * f
let to_function f (w,x) = get x (Printer.create w f)
let to_buffer b = to_function (Buffer.add_string b)
let to_channel oc = to_function (output_string oc)
let to_string z = let b = Buffer.create 32 in to_buffer b z; Buffer.contents b
let to_file fn z = let oc = open_out fn in to_channel oc z; close_out oc
let print ppf = to_function (Format.pp_print_string ppf)



let printf_k fmt_s (acc : (unit, t) CamlinternalFormat.acc) =
  let open CamlinternalFormat in
  let l = ref empty in
  let add x = l := conc !l x in
  let stack = ref [] in
  let push x = stack := (x,!l) :: !stack; l := empty in
  let err msg =
    failwith (Printf.sprintf "Mlfi_pp.printf: invalid format string %S. %s" fmt_s msg)
  in
  let pop () = match !stack with
    | (x,old) :: st -> stack := st; let nl = !l in l := old; (x,nl)
    | _ -> err "No matching box opening"
  in
  let rec k = function
    | Acc_string_literal(p, s)
    | Acc_data_string(p, s) -> k p; add (str s)
    | Acc_char_literal(p, c)
    | Acc_data_char(p, c) -> k p; add (str (String.make 1 c))
    | Acc_delay(p, f) -> k p; add f
    | Acc_flush p -> k p
    | Acc_invalid_arg (_, msg) -> invalid_arg msg
    | Acc_formatting_lit (p, lit) ->
        k p;
        formatting (string_of_formatting_lit lit)
    | Acc_formatting_gen (p, x) ->
        k p;
        begin match x with
        | Acc_open_box _ -> push (`BoxLeft 2)
        | Acc_open_tag _ -> ()
        end
    | End_of_acc -> ()
  and formatting = function
    | "@]" ->
        begin match pop () with
        | (`BoxLeft k,u) -> add (indent k u)
        | _ -> err "@] doesn't match a @["
        end
    | "@<" -> push `LineLeft
    | "@>" ->
        begin match pop () with
        | `LineLeft, u -> add (line u)
        | _ -> err "@> doesn't match a @>"
        end
    | s ->
        add (str s)
  in
  k acc;
  if !stack <> [] then err "Some boxes are not closed";
  !l

let printf (CamlinternalFormatBasics.Format (fmt, fmt_s) : ('a, unit, t, f) format4) : 'a =
  let open CamlinternalFormat in
  make_printf (fun () acc -> printf_k fmt_s acc) () End_of_acc fmt

module Operators =
struct
  let (~~) fmt = printf fmt
  let (//) = either
  let (///) s1 s2 = str s1 // str s2
  let (++) = conc
end
