(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

(** Csml.*)

val csharp_available: bool ref
    (** Is the .Net runtime available in the current process. *)


type cshandle
exception Csharp_exception of string * string * cshandle
    (** A C# exception wrapped as an OCaml exception.
        The arguments are: the exception's type name, the exception's message,
        the exception value itself. *)

class csval: cshandle ->
  object
    method cshandle: cshandle
  end
    (** Base class for ML objects that wraps C# values *)

val loadfile: string -> unit
    (** Dynamically load a plugin. If the OCaml runtime is running in native code
        then the extension of the filename is replaced by [.cmxs]. *)

(** {2 Internal use only} *)

val find_cs2ml_callback: int -> string -> string -> 'a -> 'b
val release_cs_value: Obj.t ref
val resolve_cs2ml: (int -> string -> Obj.t) ref
val ml2cs_register: string -> 'a -> unit
val ml2cs_registered: string -> 'a
val notify_ml_stub: string -> unit
val ml_stub_available: string -> bool
val csml_pop: Obj.t ref
