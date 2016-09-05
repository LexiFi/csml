ROOT=..
include $(ROOT)/Makefile.common

.PHONY: all clean

COMPILER_SRCS=pp.mli pp.ml misc.ml lexer.mli lexer.ml ast.ml parser.ml tast.ml resolution.ml stub.ml gen_csml_iface.ml main.ml

ifeq ($(TOOLCHAIN), msvc)
  CCLIB=libcsml.lib
  EXTRACCOPTS=-ccopt -DWIN32
endif

ifeq ($(TOOLCHAIN), mingw)
  CCLIB=-lcsml csml.exp
  EXTRA_INSTALLFILES=csml.exp
  EXTRACCOPTS=-ccopt -DWIN32
endif

ifneq ($(SYSTEM), win32)
  CCLIB=-L. -lcsml
endif

all:
# Build the compiler
	$(OCAMLC) $(OCAMLFLAGS) -o csml$(EXE) $(COMPILER_SRCS)

# Automatically generate some parts of the runtime support
	./csml -gen csml_iface_generated.cs
	./csml csml_std.csml

# Produce a monolithic .cs file for the C# runtime support
	cat csml_iface.cs csml_iface_generated.cs > csml.cs
	grep -v using csml_std.cs >> csml.cs

# Compile the C runtime support
	$(OCAMLC) -c $(EXTRACCOPTS) csml.c
	$(OCAMLMKLIB) -verbose -custom -oc csml csml$(EXT_OBJ)

# Compile the OCaml runtime support
	$(OCAMLC) -c csml_iface.mli
	$(OCAMLC) -c csml_init.mli
	$(OCAMLC) -a -o csml_standalone.cma csml_iface.ml
	$(OCAMLC) -a -o csml_init.cma -cclib "$(CCLIB)" csml_init.ml csml_std.ml
	$(OCAMLOPT) -a -o csml_standalone.cmxa csml_iface.ml
	$(OCAMLOPT) -a -o csml_init.cmxa -cclib "$(CCLIB)" csml_init.ml csml_std.ml

# Build native DLLs (OCaml runtime + basic libraries + csml C/OCaml runtime support)
	$(OCAMLC) -output-obj -o libcsml_ml_byt$(EXT_DLL) -linkall $(CSMLMLLIBS)
	$(OCAMLOPT) -output-obj -o libcsml_ml_opt$(EXT_DLL) -linkall $(CSMLMLLIBS:.cma=.cmxa)

# Build C# code to link to these DLLs
	./csml -dllbind libcsml_ml_byt > csml_ml_byt.cs
	./csml -dllbind libcsml_ml_opt > csml_ml_opt.cs

# Build C# DLLs (csml C# runtime support)
	$(CSC) /target:library /out:csml_byt.dll csml_ml_byt.cs $(CSMLCSCODE)
	$(CSC) /target:library /out:csml_opt.dll csml_ml_opt.cs $(CSMLCSCODE)


GENERATED_FILES =\
	csml_ml_opt.cs csml_ml_byt.cs \
	csml_std.cs csml_std.ml \
	csml_iface_generated.cs \
	csml.cs csml$(EXE)

clean:
	rm -f *.dll *.so *.cm* *~ *.obj *.o *.lib *.a *.cds $(GENERATED_FILES)
