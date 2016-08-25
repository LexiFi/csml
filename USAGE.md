Using CSML
==========

Using the compiler
------------------

The CSML compiler is invoked with a simple command line like:

    csml myscript.csml

where `myscript.csml` is a CSML script. The compiler reads the script
(and any script referenced from it) and produces all the files
according to the `mlstub`, `cstub`, `mlfile` and `csfile`
declarations.

It is important to note that the compiler does not access any other
file. In particular, it never parse OCaml `.cmi` files and it
does not use .NET reflection to check the type of the imported
components. Instead, it produces OCaml and C# source files that
incorporate static type checking. If the C# or OCaml compiler fail on
one of these files, it means that the CSML script contains an invalid
type for some of the imported component (usually, the message issued by
the compiler is clear enough to indicate the problem).

Computing dependencies
----------------------

The following command line asks the CSML compiler to produce on its
standard output a description of the dependencies implied by the CSML
script, in a format suitable for inclusion in a Makefile:

    csml -dep myscript.csml

Linking
-------

A typical mixed C# and OCaml project is made of a number of C# and
OCaml units. Some of them are produced by the CSML compiler and some of
them are provided explicitly. There is some freedom in the way all these
units are linked together.

### Initialization

The final application is always a .NET assembly that contains some C#
parts of the application (some of them can be linked in external DLLs).
It must start by calling all the initialization methods for the CSML
scripts that are part of the application. For instance, if one of those
script has a directive like:

    csstub "cs_stub.cs" InitClass

then the main program must call `Lexifi.Interop.InitClass.Init();`.

### Static linking

In this linking strategy, the OCaml part of the application is linked
into a DLL with the `-output-obj` option of
`ocamlc` or `ocamlopt`. You must include two
libraries `csml_standalone.cma` and `csml_init.cma` into this DLL in
addition to all the libraries and user code needed by the application.
Note that this DLL contains not only the code for the application but
also the OCaml runtime system.

The C# part of the application is linked as a .NET assembly by the C#
compiler. In addition to the code of application, you must link the C#
file `csml.cs` into this assembly. It is also necessary to
link a small C# file that indicates the name of the DLL produced above.
The CSML compiler can generates such a file:

    csml -dllbind mydll.dll > mydll_ptr.cs

where `mydll.dll` is the name of the DLL with the OCaml application and
runtime system, and `mydll_ptr.cs` is the name of the C# file to be
created. This file must then be linked with `csml.cs` and the
rest of the application.

### Dynamic linking

The CSML distribution comes with two pre-linked DLLs `csml_ml_byt.dll`
and `csml_ml_opt.dll` that contain the CSML runtime library and the
OCaml runtime (in bytecode or native form). These DLLs also contains the
following OCaml libraries: `dynlink`, `bigarray`,
`unix`. The distribution also comes with two files
`csml_ml_byt.cs` and `csml_ml_opt.cs` that correspond of the output of
`csml -dllbin` applied to these DLLs.

To use these DLLs, one must link the application into one or several
files that can be linked (`.cmo` or `.cma` in
bytecode, `.cmxs` in native code). The C# part of the
application can use the built-in static method
`LexiFi.Interop.Csml.LoadFile` to load such files dynamically
(this must be done after the call to the initialization methods in C#).
This method automatically translates the `.cmo` and
`.cma` file extensions to `.cmxs` when the
underlying OCaml runtime system is the native one.

When one uses the default DLLs, one can also use `csml_byt.dll` or
`csml_opt.dll`. They are .NET DLL that contain the CSML C# runtime
(`csml.cs`) and references to the default DLLs (i.e.
`csml_ml_byt.cs` and `csml_ml_opt.cs`).

### Linking the OCaml code without the C# part

In some cases, it is useful to link the OCaml modules produced by the
CSML compiler even when the .NET runtime is not available in the current
application. In this case, calling an OCaml function that imports a C#
component will raise an exception (actually, it stops the application
currently). To link the OCaml modules produced by CSML, one must include
`csml_standalone.cma` and not `csml_init.cma` (resp. `.cmxa` in native
code).

It is possible to check at runtime on the OCaml side whether the C#
runtime is available with the Boolean `Csml_iface.csharp_available`.


A note on environment variables
-------------------------------

Windows provides two APIs to access environment variables of the
current process. As a matter of fact, the C runtime library keeps its
own copy of the environment, which is the one used by the OCaml
functions. This is problematic because the .NET framework uses the
other set of functions. As a consequence, the two environment might
get ouf of sync: a change done in C# will not be visible in OCaml. To
alleviate this problem, the initialization part of the CSML OCaml
runtime library copies the .NET environment to the OCaml one. As a
consequence, any change to the environment made before this
initialization is reflected to the OCaml side (this allow to pass some
information from C# to OCaml).
