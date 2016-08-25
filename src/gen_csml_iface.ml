(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Pp
open Pp.Operators
open Misc

let cb l = ~~"{@[\n %t@]\n }\n " (all l)

let d f x = f x x
let d4 f x = f x x x x

let comma f = seq ~sep:~~", " f 0

let typs = comma ~~"T%i"
let args = comma (d ~~"T%i arg%i")

let g x = if linear_length x = 0 then empty else ~~"<%t>" x
let (+?) x y =
  if linear_length x = 0 then y
  else if linear_length y = 0 then x
  else ~~"%t, %t" x y

let seqws f i n = seq ~sep:ws f i n ++ ws
let gen ?(from=0) f = seqws f from 16

let del r na t n = ~~"@<public delegate %s %s%t(%t);@>" r na (g t) (args n)
let arrow n = del "S" "Arrow" (typs n +? ~~"S") n
let arrow_void n = del "void" "ArrowVoid" (typs n) n

let app r t tn ret n =
  ~~"@<public static %s Apply%t(%t) {@[\n %sf(%t);@]\n }@>"
    r (g t) (~~"%s%t f" tn (g t) +? args n) ret (comma ~~"arg%i" n)

let apply n = app "S" (typs n +? ~~"S") "Arrow" "return " n
let apply_void n = app "void" (typs n) "ArrowVoid" "" n


let tuple n =
  ~~"@<public class Tuple%t %t@>"
    (g (typs n))
    (cb  [
       ~~"public Tuple(%t) %t"
         (args n) (cb [ seqws (d ~~"val%i = arg%i;") 0 n ]);
       seqws (d ~~"private T%i val%i;") 0 n;
       seqws (d4 ~~"public T%i TVal%i { get { return val%i; } set { val%i = value; } }") 0 n ;

     ])

let reg n =
  all [
    ~~"@<public delegate System.IntPtr cs2ml_typ%i(%t);@>"
      n (comma ~~"System.IntPtr x%i" n);
    ws;
    ~~"@<[System.Runtime.InteropServices.DllImport(Csml.dllname, EntryPoint=\"csml_to_int_ptr\")]@>";
    ws;
    ~~"@<public static extern System.IntPtr ToIntPtr(cs2ml_typ%i f);@>"
      n;
  ]

let csml_callback n =
  all [
    ~~"@<[System.Runtime.InteropServices.DllImport(Csml.dllname)]@>\n ";
    ~~"@<public static extern System.IntPtr csml_apply%i(System.IntPtr f%s %t);@>\n " n (if n = 0 then "" else ", ") (comma ~~"System.IntPtr x%i" n);
  ]

let csml_push n =
  all [
  ~~"@<[System.Runtime.InteropServices.DllImport(Csml.dllname)]@>\n ";
  ~~"@<public static extern void csml_push_%i(%t);@>\n " n (comma ~~"System.IntPtr x%i" n);
]


let create () =
  all [
    ~~"namespace %s " ns;
    cb [
      gen arrow;
      gen arrow_void;
      ~~"public partial class %s " clname;
      cb [
        gen apply;
        gen apply_void;
        ~~"@<public static void Apply<T0>(ArrowVoid f, T0 arg0) {@[\n f();@]\n }@>";
        ~~"@<public static S Apply<T0, S>(Arrow<S> f, T0 arg0) {@[\n return f();@]\n }@>";
        gen ~from:1 reg;
        gen ~from:0 csml_callback;
        gen ~from:2 csml_push;
      ];
      gen ~from:2 tuple;
    ]
  ]
