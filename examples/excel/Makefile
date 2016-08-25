## This directory illustrate calling OCaml functions from Excel (going
## through a .NET assembly and its COM wrapper registered to the registry).
##
## To prepare the demo, run "make build" in a shell started with
## administrative privileges.
##
## Then open the csml.xlsm file (and enable macros).


ROOT = ../..
NAME = excel

GENERATED_FILES = \
  excel_csml.cs excel_csml.ml

MLCODE = \
  excel.ml excel_csml.ml

CSCODE = \
  excel_csml.cs

DLLNAME = \
  csml_excel.dll

include $(ROOT)/Makefile.examples

CSC += /t:library

# Demo only in standalone+native mode

REGASM = /cygdrive/c/WINDOWS/Microsoft.NET/Framework/v2.0.50727/RegAsm.exe
# To be adjusted
# Note: running RegAsm requires administrative rights

build: build_standalone_opt
	$(CSC) /out:$(DLLNAME) $(NAME)_ml_opt.cs $(CSMLCSCODE) $(CSCODE)
	$(REGASM) /tlb /codebase $(DLLNAME)

clean::
	-$(REGASM) /tlb /unregister $(DLLNAME)
	rm -f $(DLLNAME) $(DLLNAME:.dll=.tlb)
