/* This file is released under the terms of an MIT-like license.     */
/* See the attached LICENSE file.                                    */
/* Copyright 2016 by LexiFi.                                         */

#include <stdio.h>
#include <assert.h>
#include <malloc.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <string.h>

#undef PROFILE_CSML

#ifdef WIN32
#ifdef __GNUC__
#define WINVER 0x501
#define  __MSVCRT_VERSION__ 0x0700
#endif

#include <wtypes.h>
#include <objbase.h>
#include <windows.h>

#define EXPORT __declspec(dllexport)

#else

/* Linux */

#define EXPORT
#define __stdcall
#include <pthread.h>
#endif


static value* get_caml_callback(char *s) {
  value *f = caml_named_value(s);
  if (!f) {
    printf("Cannot find ML value %s\n", s);
    fflush(stdout);
    exit(1);
  }
  return f;
}

/* These functions are provided by the ML runtime. Unfortunately, they
   are not exported in the ML header files. */

value caml_make_vect(value len, value init);
value caml_array_unsafe_get(value array, value index);
value caml_array_unsafe_set(value array, value index, value newval);

/* Call C# functions from ML */

typedef value (__stdcall *ptrfun1) (value);
typedef value (__stdcall *ptrfun2) (value, value);
typedef value (__stdcall *ptrfun3) (value, value, value);
typedef value (__stdcall *ptrfun4) (value, value, value, value);
typedef value (__stdcall *ptrfun5) (value, value, value, value, value);
typedef value (__stdcall *ptrfun6) (value, value, value, value, value, value);
typedef value (__stdcall *ptrfun7) (value, value, value, value, value, value, value);
typedef value (__stdcall *ptrfun8) (value, value, value, value, value, value, value, value);
typedef value (__stdcall *ptrfun9) (value, value, value, value, value, value, value, value, value);
typedef value (__stdcall *ptrfun10)(value, value, value, value, value, value, value, value, value, value);
typedef value (__stdcall *ptrfun11)(value, value, value, value, value, value, value, value, value, value, value);

#define check_exn(r)                                                    \
    if (Is_exception_result(r))                                         \
      caml_raise(Extract_exception(r));                                 \
    return r;                                                           \

#define mayraise(...) {                                                 \
    value r = f(__VA_ARGS__);                                           \
    check_exn(r);                                                       \
  }

value cs2ml_apply1 (ptrfun1 f, value x1) mayraise(x1)
value cs2ml_apply2 (ptrfun2 f, value x1, value x2) mayraise(x1, x2)
value cs2ml_apply3 (ptrfun3 f, value x1, value x2, value x3) mayraise(x1, x2, x3)
value cs2ml_apply4 (ptrfun4 f, value x1, value x2, value x3, value x4) mayraise(x1, x2, x3, x4)
value cs2ml_apply5 (ptrfun5 f, value x1, value x2, value x3, value x4, value x5) mayraise(x1, x2, x3, x4, x5)
value cs2ml_apply6 (ptrfun6 f, value x1, value x2, value x3, value x4, value x5, value x6) mayraise(x1, x2, x3, x4, x5, x6)
value cs2ml_apply7 (ptrfun7 f, value x1, value x2, value x3, value x4, value x5, value x6, value x7) mayraise(x1, x2, x3, x4, x5, x6, x7)
value cs2ml_apply8 (ptrfun8 f, value x1, value x2, value x3, value x4, value x5, value x6, value x7, value x8) mayraise(x1, x2, x3, x4, x5, x6, x7, x8)
value cs2ml_apply9 (ptrfun9 f, value x1, value x2, value x3, value x4, value x5, value x6, value x7, value x8, value x9) mayraise(x1, x2, x3, x4, x5, x6, x7, x8, x9)
value cs2ml_apply10(ptrfun10 f, value x1, value x2, value x3, value x4, value x5, value x6, value x7, value x8, value x9, value x10) mayraise(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
value cs2ml_apply11(ptrfun11 f, value x1, value x2, value x3, value x4, value x5, value x6, value x7, value x8, value x9, value x10, value x11) mayraise(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)

value cs2ml_apply5_byte(value * argv, int argn) {
  value r = ((ptrfun5)argv[0]) (argv[1], argv[2], argv[3], argv[4], argv[5]);
  check_exn(r);
}
value cs2ml_apply6_byte(value * argv, int argn) {
  value r = ((ptrfun6)argv[0]) (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
  check_exn(r);
}
value cs2ml_apply7_byte(value * argv, int argn) {
  value r = ((ptrfun7)argv[0]) (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
  check_exn(r);
}
value cs2ml_apply8_byte(value * argv, int argn) {
  value r = ((ptrfun8)argv[0]) (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]);
  check_exn(r);
}
value cs2ml_apply9_byte(value * argv, int argn) {
  value r = ((ptrfun9)argv[0]) (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9]);
  check_exn(r);
}
value cs2ml_apply10_byte(value * argv, int argn) {
  value r = ((ptrfun10)argv[0]) (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
  check_exn(r);
}

value cs2ml_apply11_byte(value * argv, int argn) {
  value r = ((ptrfun11)argv[0]) (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10], argv[11]);
  check_exn(r);
}

/* Wrapped ML values */

#ifdef PROFILE_CSML
static int csml_nwrapped_values = 0;
#endif

/* A C# finalizer can be run in any thread. We need to be very careful
   about race conditions! A previous version was using a lock-free data structure (Interlocked Singly Linked List)
   to store the list of wrapped MLFi values to be released (See http://msdn2.microsoft.com/en-us/library/ms684121.aspx).
   We switched to using locks (mutexes) in order to restore compatibility with Win2000. */

static value **mlvalues_tofree = NULL;
static int mlvalues_tofree_nb = 0;
static int mlvalues_tofree_size = 0;

#ifdef WIN32
static HANDLE mlvalues_tofree_mutex = NULL;
#define LOCK WaitForSingleObject(mlvalues_tofree_mutex, INFINITE)
#define UNLOCK ReleaseMutex(mlvalues_tofree_mutex)
#else
static pthread_mutex_t mlvalues_tofree_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK pthread_mutex_lock(&mlvalues_tofree_mutex)
#define UNLOCK pthread_mutex_unlock(&mlvalues_tofree_mutex)
#endif

static void reclaim_ocaml_values() {
  LOCK;

  while (mlvalues_tofree_nb > 0) {
    mlvalues_tofree_nb--;
    caml_remove_global_root(mlvalues_tofree[mlvalues_tofree_nb]);
    free(mlvalues_tofree[mlvalues_tofree_nb]);
#ifdef PROFILE_CSML
    csml_nwrapped_values--;
    if (csml_nwrapped_values % 100 == 0) {
      printf("[csml] wrapped OCaml values = %d\n", csml_nwrapped_values);
      fflush(stdout);
    }
#endif
  }

  UNLOCK;
}

/* This function can be called from an arbitrary thread by the C# finalizer. */
EXPORT void __stdcall release_ocaml_value(value *gr) {
  LOCK;

  if (mlvalues_tofree_nb >= mlvalues_tofree_size) {
    int nsize = mlvalues_tofree_size * 2 + 5;
    mlvalues_tofree = realloc(mlvalues_tofree, nsize * sizeof(value*));
    mlvalues_tofree_size = nsize;
  }

  mlvalues_tofree[mlvalues_tofree_nb] = gr;
  mlvalues_tofree_nb++;

  UNLOCK;
}

static void initialize_mlvalues_tofree() {
  mlvalues_tofree_nb = 0;
  mlvalues_tofree_size = 0;
  mlvalues_tofree = NULL;
#ifdef WIN32
  mlvalues_tofree_mutex = CreateMutex(0, FALSE, NULL);
  assert(mlvalues_tofree_mutex);
#endif
}

EXPORT value* __stdcall wrap_ocaml_value(value x) {
  /* This function cannot trigger the OCaml GC. */
  value *gr;

  /* Flush the set of pending releases. */
  reclaim_ocaml_values();

  gr = (value*) malloc(sizeof(value));
  *gr = x;
  caml_register_global_root(gr);
#ifdef PROFILE_CSML
    csml_nwrapped_values++;
    if (csml_nwrapped_values % 100 == 0) {
      printf("[csml] wrapped OCaml values = %d\n", csml_nwrapped_values);
      fflush(stdout);
    }
#endif

  return gr;
}

/* Wrapped C# values */

#define CShandle_val(v) (*((value*) Data_custom_val(v)))
typedef void (__stdcall *cs2ml_typ_release_cs_value)(value);
static cs2ml_typ_release_cs_value cs2ml_fptr_release_cs_value = NULL;

typedef value (__stdcall *cs2ml_typ_resolve_cs2ml)(value);
static cs2ml_typ_resolve_cs2ml cs2ml_fptr_resolve_cs2ml = NULL;

void finalize_wrapped_cs_value(value v) {
  assert(cs2ml_fptr_release_cs_value);
  if (CShandle_val(v) != (-1)) {
    cs2ml_fptr_release_cs_value(CShandle_val(v));
    CShandle_val(v) = (-1);
  }
}

value release_cs_value(value v) {
  if (v != Val_unit)
    finalize_wrapped_cs_value(v);
  return Val_unit;
}

value resolve_cs2ml(value v) {
  assert(cs2ml_fptr_resolve_cs2ml);
  v = cs2ml_fptr_resolve_cs2ml(v);
  if (v == 0) return Val_unit;
  else return v;
}


static struct custom_operations wrapped_cs_value_ops = {
  "Lexifi.Interop.WrappedCSharpValue",
  finalize_wrapped_cs_value,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* Stack of ML values */

static value mlstack = Val_unit;

EXPORT value* __stdcall csml_mlstack() {
  return &mlstack;
}

EXPORT void __stdcall csml_push(value v) {
  CAMLparam1(v);
  value c = caml_alloc_tuple(2);
  Store_field(c,0,v);
  Store_field(c,1,mlstack);
  mlstack = c;
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_2(value v1, value v2) {
  CAMLparam2(v1, v2);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_3(value v1, value v2, value v3) {
  CAMLparam3(v1, v2, v3);
  csml_push(v3);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_4(value v1, value v2, value v3, value v4) {
  CAMLparam4(v1, v2, v3, v4);
  csml_push(v4);
  csml_push(v3);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_5(value v1, value v2, value v3, value v4, value v5) {
  CAMLparam5(v1, v2, v3, v4, v5);
  csml_push(v5);
  csml_push(v4);
  csml_push(v3);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_6(value v1, value v2, value v3, value v4, value v5, value v6) {
  CAMLparam5(v1, v2, v3, v4, v5);
  CAMLxparam1(v6);
  csml_push(v6);
  csml_push(v5);
  csml_push(v4);
  csml_push(v3);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_7(value v1, value v2, value v3, value v4, value v5, value v6, value v7) {
  CAMLparam5(v1, v2, v3, v4, v5);
  CAMLxparam2(v6, v7);
  csml_push(v7);
  csml_push(v6);
  csml_push(v5);
  csml_push(v4);
  csml_push(v3);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_8(value v1, value v2, value v3, value v4, value v5, value v6, value v7, value v8) {
  CAMLparam5(v1, v2, v3, v4, v5);
  CAMLxparam3(v6, v7, v8);
  csml_push(v8);
  csml_push(v7);
  csml_push(v6);
  csml_push(v5);
  csml_push(v4);
  csml_push(v3);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_9(value v1, value v2, value v3, value v4, value v5, value v6, value v7, value v8, value v9) {
  CAMLparam5(v1, v2, v3, v4, v5);
  CAMLxparam4(v6, v7, v8, v9);
  csml_push(v9);
  csml_push(v8);
  csml_push(v7);
  csml_push(v6);
  csml_push(v5);
  csml_push(v4);
  csml_push(v3);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT void __stdcall csml_push_10(value v1, value v2, value v3, value v4, value v5, value v6, value v7, value v8, value v9, value v10) {
  CAMLparam5(v1, v2, v3, v4, v5);
  CAMLxparam5(v6, v7, v8, v9, v10);
  csml_push(v10);
  csml_push(v9);
  csml_push(v8);
  csml_push(v7);
  csml_push(v6);
  csml_push(v5);
  csml_push(v4);
  csml_push(v3);
  csml_push(v2);
  csml_push(v1);
  CAMLreturn0;
}

EXPORT value __stdcall csml_pop() {
  value v = Field(mlstack,0);
  mlstack = Field(mlstack,1);
  return v;
}

value csml_pop_from_ml(value unit) {
  value v;
  v = Field(mlstack,0);
  mlstack = Field(mlstack,1);
  return v;
}

EXPORT void __stdcall csml_create(int tag, int size) {
  csml_push(size ? caml_alloc(size,tag) : Atom(tag));
}

EXPORT void __stdcall csml_dup() {
  csml_push(Field(mlstack,0));
}

EXPORT void __stdcall csml_setfield_imm(int pos, value v) {
  Store_field(Field(mlstack,0),pos,v);
}

EXPORT void __stdcall csml_setfield(int pos) {
  value v = Field(mlstack,0);
  mlstack = Field(mlstack,1);
  Store_field(Field(mlstack,0),pos,v);
}

EXPORT void __stdcall csml_setfield_swap(int pos) {
  value v = Field(mlstack,0);
  mlstack = Field(mlstack,1);
  Store_field(Field(mlstack,0),pos,v);
  Store_field(mlstack,0,v);
}

EXPORT char* __stdcall csml_create_string(int size) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_string(size);
  csml_push(v);
  CAMLreturnT(char*,String_val(v));
}

EXPORT void __stdcall csml_create_double(double x) {
  csml_push(caml_copy_double(x));
}

EXPORT void __stdcall csml_create_int64(long long x) {
  csml_push(caml_copy_int64(x));
}

EXPORT value __stdcall csml_cs_wrapper(value i) {
  value v = caml_alloc_custom(&wrapped_cs_value_ops,sizeof(value),1,1000); /* TODO: tweak that */
  CShandle_val(v) = i;
  return v;
}

EXPORT void __stdcall csml_push_cs_wrapper(value i) {
  csml_push(csml_cs_wrapper(i));
}

EXPORT void __stdcall csml_get_ml_value(value *gr) {
  csml_push(*gr);
}


/* Inspect OCaml values */

EXPORT long long __stdcall csml_int64_val(value v) { return Int64_val(v); }
EXPORT double __stdcall csml_double_val(value v) { return Double_val(v); }
EXPORT long long __stdcall csml_wosize_val(value v) { return Wosize_val(v); }
EXPORT value __stdcall csml_cshandle_val(value v) {
  /* note: using 'value' as return type to ensure 64-bit under Win64. cf #4554. */
  if (Tag_val(v) == Object_tag) {
    static long methodid = 0;
    if (methodid == 0)  methodid = caml_hash_variant("cshandle");
    v = caml_callback(caml_get_public_method(v, methodid), v);
  }
  if (v == Val_unit)
    return (-2);
  assert(Tag_val(v) == Custom_tag);
  return CShandle_val(v);
}


/* Main entry point into the OCaml engine */

EXPORT void __stdcall csml_init(cs2ml_typ_release_cs_value release_cs_value, cs2ml_typ_resolve_cs2ml resolve_cs2ml, char **argv) {
  cs2ml_fptr_release_cs_value = release_cs_value;
  cs2ml_fptr_resolve_cs2ml = resolve_cs2ml;
  initialize_mlvalues_tofree();

  caml_register_global_root(&mlstack);

  /* Now start the OCaml runtime */
  caml_startup(argv);
}

EXPORT void* __stdcall csml_malloc(int n) {
  return malloc(n);
}

static int csml_initialized = 0;
EXPORT int __stdcall csml_is_initialized() {
  if (csml_initialized != 0) return 1;
  csml_initialized = 1;
  return 0;
}


/* Call OCaml functions from C# */

EXPORT value __stdcall csml_apply0(value f) {
  return caml_callback_exn(f, Val_unit);
}
EXPORT value __stdcall csml_apply1(value f, value arg0) {
  return caml_callback_exn(f, arg0);
}
EXPORT value __stdcall csml_apply2(value f, value arg0, value arg1) {
  return caml_callback2_exn(f, arg0, arg1);
}
EXPORT value __stdcall csml_apply3(value f, value arg0, value arg1, value arg2) {
  return caml_callback3_exn(f, arg0, arg1, arg2);
}
EXPORT value __stdcall csml_apply4(value f, value arg0, value arg1, value arg2, value arg3) {
  value args[] = {arg0, arg1, arg2, arg3};
  return caml_callbackN_exn(f, 4, args);
}
EXPORT value __stdcall csml_apply5(value f, value arg0, value arg1, value arg2, value arg3, value arg4) {
  value args[] = {arg0, arg1, arg2, arg3, arg4};
  return caml_callbackN_exn(f, 5, args);
}
EXPORT value __stdcall csml_apply6(value f, value arg0, value arg1, value arg2, value arg3, value arg4, value arg5) {
  value args[] = {arg0, arg1, arg2, arg3, arg4, arg5};
  return caml_callbackN_exn(f, 6, args);
}
EXPORT value __stdcall csml_apply7(value f, value arg0, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6) {
  value args[] = {arg0, arg1, arg2, arg3, arg4, arg5, arg6};
  return caml_callbackN_exn(f, 7, args);
}
EXPORT value __stdcall csml_apply8(value f, value arg0, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6, value arg7) {
  value args[] = {arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7};
  return caml_callbackN_exn(f, 8, args);
}
EXPORT value __stdcall csml_apply9(value f, value arg0, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6, value arg7, value arg8) {
  value args[] = {arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8};
  return caml_callbackN_exn(f, 9, args);
}
EXPORT value __stdcall csml_apply10(value f, value arg0, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6, value arg7, value arg8, value arg9) {
  value args[] = {arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9};
  return caml_callbackN_exn(f, 10, args);
}
EXPORT value __stdcall csml_apply11(value f, value arg0, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6, value arg7, value arg8, value arg9, value arg10) {
  value args[] = {arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10};
  return caml_callbackN_exn(f, 11, args);
}


static value *csharp_exception = NULL;
#define check_csharp_exception() if (!csharp_exception) csharp_exception = get_caml_callback("csharp_exception")

EXPORT void __stdcall csml_push_exn(value i) {
  CAMLparam0();
  CAMLlocal1(exn);
  check_csharp_exception();

  exn = caml_alloc(4,0);
  Store_field(exn, 0, *csharp_exception);
  Store_field(exn, 1, csml_pop());
  Store_field(exn, 2, csml_pop());
  Store_field(exn, 3, csml_cs_wrapper(i));
  csml_push(exn);
  CAMLreturn0;
}

EXPORT void __stdcall csml_raise(value i) {
  csml_push_exn(i);
  caml_raise(csml_pop());
}

EXPORT value __stdcall csml_extract_cs_exn(value v) {
  check_csharp_exception();
  if (Field(v,0) == *csharp_exception)
    return CShandle_val(Field(v,3));
  else
    return 0;
}

EXPORT int __stdcall csml_array_length(value v) {
  return
    (Tag_val(v) == Double_array_tag ? Wosize_val(v) / Double_wosize :
     Wosize_val(v));
}

EXPORT value __stdcall csml_array_get(value v, int i) {
  return caml_array_unsafe_get(v, Val_int(i));
}

EXPORT void __stdcall csml_push_array(int size) {
  csml_push(caml_make_vect(Val_int(size),csml_pop()));
}

EXPORT void __stdcall csml_array_set(int i) {
  value vi = Field(mlstack,0);
  mlstack = Field(mlstack,1);
  caml_array_unsafe_set(Field(mlstack,0), Val_int(i), vi);
}

EXPORT value* __stdcall csml_get_caml_callback(char *s) {
  value *f = caml_named_value(s);
  if (!f) {
    fprintf(stderr, "Cannot find ML value %s\n", s); fflush(stderr);
    exit(2);
  }
  return f;
}

EXPORT value __stdcall csml_to_int_ptr(value i) {
  return i;
}

/* Get the Win32 environment (not the "libc" one). */

#ifdef WIN32
value csml_get_win32_environ() {
  wchar_t *env = NULL;
  wchar_t *tmp;
  int nSizeW;
  int nSizeA;
  value res;

  /* Note: GetEnvironmentStringsA returns OEM characters, not ANSI chars!
     We need to go through the Unicode version and convert back to ANSI chars. */
  env = GetEnvironmentStringsW();
  tmp = env;
  while ( *tmp != L'\0' ) if ( *++tmp == L'\0' ) tmp++;
  nSizeW = (int)(tmp - env + 1);

  nSizeA = WideCharToMultiByte(CP_ACP, 0, env, nSizeW, NULL, 0, NULL, NULL);
  res = caml_alloc_string(nSizeA);
  WideCharToMultiByte(CP_ACP, 0, env, nSizeW, (void*) res, nSizeA, NULL, NULL);
  FreeEnvironmentStringsW(env);
  return res;
}
#else
value csml_get_win32_environ() {
  return caml_alloc_string(0);
}
#endif
