ROOT = ../..
NAME = winforms

GENERATED_FILES=\
  $(NAME).cs $(NAME).ml \
  winforms_test_stub.ml winforms_test_stub.cs

MLCODE=$(NAME).ml winforms_test.ml winforms_test_stub.ml
CSCODE=$(NAME).cs winforms_test.cs winforms_test_stub.cs

CSMLS = winforms.csml winforms_test.csml

include $(ROOT)/Makefile.examples

#EXTRACSOPTS=/r:System.Windows.Forms.dll /r:System.Drawing.dll
