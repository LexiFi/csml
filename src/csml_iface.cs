/* This file is released under the terms of an MIT-like license.     */
/* See the attached LICENSE file.                                    */
/* Copyright 2016 by LexiFi.                                         */

using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using LexiFi.Common;

namespace LexiFi.Common {
  using System.Runtime.Serialization;

  public class LexiFi_exception: Exception{
    public LexiFi_exception(string arg):base(arg){}
    public LexiFi_exception():base(){}
    protected LexiFi_exception(SerializationInfo info, StreamingContext context):base(info,context){}
  }
}

namespace LexiFi.Interop {
  public class CSValueKilledException : LexiFi_exception {
    public CSValueKilledException() : base() { }
  }
  public class MLValueKilledException : LexiFi_exception {
    public MLValueKilledException() : base() { }
  }
  public class OptionNoneException : LexiFi_exception {
    public OptionNoneException(Type T) : base(T.ToString()) {}
  }
  public class Option<T> {
    public Option(T x) { val = x; is_some = true; }
    public Option() { is_some = false; }
    public bool Is_some{ get{ return is_some; } }
    public void Clear() { val = default(T); is_some = false; }
    public T Val {
      get{
        if (is_some) return val;
        else throw new OptionNoneException(typeof(T));
      }
      set { val = value; is_some = true; }
    }
    public override bool Equals(object obj) {
      Option<T> opt = obj as Option<T>;
      if (opt == null) return false;

      if (is_some)
          return (opt.is_some && val.Equals(opt.val));
      else
          return !opt.is_some;
    }
    public override int GetHashCode() {
        if (is_some)
            return val.GetHashCode();
        else
            return 0; // all "None"s are equal so give them a zero hash code
    }
    private T val;
    private bool is_some;
  }

  public class Callback {
    public IntPtr handle;
    ~Callback() { Csml.release_ocaml_value(handle); }
    public Callback(IntPtr v) {handle = Csml.wrap_ocaml_value(v);}
    public void cs2ml() { Csml.csml_push(Csml.csml_get_mlval(handle)); }
  }

  public class MLException : LexiFi_exception {
    public IntPtr handle;
    string msg = null;
    ~MLException() { Csml.release_ocaml_value(handle); }
    public MLException(IntPtr v, string c) : base(c)
      { handle = Csml.wrap_ocaml_value(v); }

    public static Exception Create(IntPtr v) {
      IntPtr exn = LexiFi.Interop.Csml.csml_extract_cs_exn(v);
      if ((long) exn != 0) {
        // The ML exn is actually a wrapped C# exn. Unwrap it.
        return (Exception) (System.Runtime.InteropServices.GCHandle.FromIntPtr(exn).Target);
      } else {
        // Wrap the ML exn as a C# exn.
        string c = Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(Marshal.ReadIntPtr(v)));
        return new MLException(v,c);
      }
    }
    override public string Message {
      get {
        if (msg == null) {
          try { msg = Csml.PrintException(this); }
          catch (Exception exn) {
            Csml.WriteLine("Error while computing error message for ML exception:");
            Csml.WriteLine(exn.Message);
            Csml.WriteLine(exn.StackTrace);
            msg = "ML Error";
          }
        }
        return msg;
      }
    }
  }

  public partial class Csml {

    public static Object csml_lock = new Object();

    public static bool silent = (Environment.GetEnvironmentVariable("MLFI_SILENT") != null);

    public static void WriteLine(string s) {
      if (!silent) System.Console.WriteLine(s);
    }

    public class ArrowVoidWrapper {
      private ArrowVoid f;
      public ArrowVoidWrapper(ArrowVoid f) {
        this.f = f;
      }
      public void Run() {
        f();
      }
    }

    public static Delegate to_delegate(Type t, ArrowVoid f) {
      ArrowVoidWrapper w = new ArrowVoidWrapper(f);
      return Delegate.CreateDelegate(t, w, "Run");
    }

    public static bool debug_stacktrace = (Environment.GetEnvironmentVariable("CSML_DEBUG_STACKTRACE") != null);

    public static void csml_push_string(string s) {
      int len = (s == null ? 0 : s.Length);
      IntPtr p = csml_create_string(len);
      for (int i = 0; i < len; i++) Marshal.WriteByte(p,i,(byte)s[i]);
    }
    public static void csml_push_blob(byte[] s) {
      int len = (s == null ? 0 : s.Length);
      IntPtr p = csml_create_string(len);
      for (int i = 0; i < len; i++) Marshal.WriteByte(p,i,s[i]);
    }
    public static int csml_nb_fields(IntPtr s) {
      return (int) csml_wosize_val(s);
    }
    public static string csml_copy_string(IntPtr s) {
      int len = (int)(csml_wosize_val(s) * IntPtr.Size - 1);
      len -= Marshal.ReadByte(s,len);
      char[] r = new Char[len];
      for (int i = 0; i < len; i++) r[i] = (char) Marshal.ReadByte(s, i);
      return new String(r);
    }
    public static byte[] csml_copy_blob(IntPtr s) {
      int len = (int)(csml_wosize_val(s) * IntPtr.Size - 1);
      len -= Marshal.ReadByte(s,len);
      byte[] r = new byte[len];
      for (int i = 0; i < len; i++) r[i] = Marshal.ReadByte(s, i);
      return r;
    }
    public static void csml_push_exception(Exception e) {
      if (e is MLException) {
        // Unwrap
        csml_push(Marshal.ReadIntPtr(((MLException)e).handle));
      } else {
        if (debug_stacktrace) {
          Csml.WriteLine("STACKTRACE:\n" + e.StackTrace + "\n=====================\n");
          Csml.WriteLine("MESSAGE:\n" + e.Message + "\n=====================\n");
        }
        // Wrap
        csml_push_string(e.Message);
        csml_push_string(e.GetType().FullName);
        csml_push_exn(GCHandle.ToIntPtr(GCHandle.Alloc(e)));
      }
    }
    public static IntPtr csml_create_exception(Exception e) {
      csml_push_exception(e);
      return csml_pop();
    }
    public static object csml_get_csval(IntPtr v) {
      if (v == (IntPtr) 1)
        return null;

      IntPtr i = csml_cshandle_val(v);
      if ((long) i == -2) return null;
      if ((long) i == -1) throw new CSValueKilledException();
      return GCHandle.FromIntPtr(i).Target;
    }
    public static IntPtr csml_get_mlval(IntPtr v) {
      if ((long) v == 0) throw new MLValueKilledException();
      return Marshal.ReadIntPtr(v);
    }

    public static void csml_push_cs_value(Object o, GCHandleType weak) {
      if (o == null)
        csml_push((IntPtr) 1);
      else
        csml_push_cs_wrapper(GCHandle.ToIntPtr(GCHandle.Alloc(o, weak)));
    }

    public static IntPtr csml_cs_value(Object o, GCHandleType weak) {
      if (o == null)
        return ((IntPtr) 1);
      else
        return csml_cs_wrapper(GCHandle.ToIntPtr(GCHandle.Alloc(o, weak)));
    }

    [DllImport(dllname)]
    private static extern IntPtr csml_malloc(int n);
    [DllImport(dllname)]
    private static extern void csml_init(release_cs_value_typ release_cs_value, resolve_cs2ml_typ resolve_cs2ml, IntPtr args);
    [DllImport(dllname)]
    public static extern IntPtr wrap_ocaml_value(IntPtr x0);
    [DllImport(dllname)]
    public static extern void release_ocaml_value(IntPtr x0);
    [DllImport(dllname)]
    public static extern IntPtr csml_pop();
    [DllImport(dllname)]
    public static extern void csml_push(IntPtr x0);
    [DllImport(dllname)]
    public static extern void csml_create(int x0,int x1);
    [DllImport(dllname)]
    public static extern IntPtr csml_create_string(int x0);
    [DllImport(dllname)]
    public static extern void csml_create_double(double x0);
    [DllImport(dllname)]
    public static extern void csml_create_int64(long x0);
    [DllImport(dllname)]
    public static extern void csml_setfield_imm(int x0,IntPtr x1);
    [DllImport(dllname)]
    public static extern void csml_setfield(int x0);
    [DllImport(dllname)]
    public static extern void csml_setfield_swap(int x0);
    [DllImport(dllname)]
    public static extern void csml_dup();
    [DllImport(dllname)]
    public static extern IntPtr csml_cs_wrapper(IntPtr x0);
    [DllImport(dllname)]
    public static extern void csml_push_cs_wrapper(IntPtr x0);
    [DllImport(dllname)]
    public static extern long csml_int64_val(IntPtr x0);
    [DllImport(dllname)]
    public static extern long csml_wosize_val(IntPtr x0);
    [DllImport(dllname)]
    public static extern double csml_double_val(IntPtr x0);
    [DllImport(dllname)]
    public static extern IntPtr csml_cshandle_val(IntPtr x0);
    [DllImport(dllname)]
    public static extern void csml_raise(IntPtr x0);
    [DllImport(dllname)]
    public static extern void csml_push_exn(IntPtr x0);
    [DllImport(dllname)]
    public static extern IntPtr csml_extract_cs_exn(IntPtr x0);
    [DllImport(dllname)]
    public static extern int csml_array_length(IntPtr x0);
    [DllImport(dllname)]
    public static extern IntPtr csml_array_get(IntPtr x0,int x1);
    [DllImport(dllname)]
    public static extern void csml_array_set(int x0);
    [DllImport(dllname)]
    public static extern void csml_push_array(int x0);
    [DllImport(dllname)]
    public static extern IntPtr csml_get_caml_callback(string s);
    [DllImport(dllname)]
    public static extern IntPtr csml_mlstack();
    [DllImport(dllname)]
    public static extern int csml_wait_for_socket(int s);
    [DllImport(dllname)]
    public static extern int csml_get_max_client_sockets();
    [DllImport(dllname)]
    public static extern int csml_get_invalid_socket();
    [DllImport(dllname)]
    public static extern int csml_wait_for_client_connection(int s, [In, Out] int[] client_socks, [In, Out] int[] client_times);
    [DllImport(dllname)]
    public static extern int csml_is_initialized();
    [DllImport(dllname)]
    public static extern void csml_set_locale(int category, string locale);

    public static IntPtr mlstack;

    private delegate void release_cs_value_typ(IntPtr v);
    private static release_cs_value_typ cs2ml_fptr_release_cs_value =
      delegate(IntPtr i) { System.Runtime.InteropServices.GCHandle.FromIntPtr(i).Free(); };

    private delegate IntPtr resolve_cs2ml_typ(IntPtr v);
    private static resolve_cs2ml_typ cs2ml_fptr_resolve_cs2ml =
      delegate(IntPtr v) { return CS2MLResolver(csml_copy_string(v)); };

    private static bool inited = false;
    public static string[] Args = new string[]{ };
    public static string ExecutableName = System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName;

    public static void Init() {
      if (inited) { return; }
      inited = true;
      mlstack = csml_mlstack();

      bool csml_inited = csml_is_initialized() != 0;
      if (!csml_inited) {
        string[] real_args = new string[1 + Args.Length];
        real_args[0] = ExecutableName;
        Args.CopyTo(real_args, 1);

        int argslen = IntPtr.Size;
        foreach (string s in real_args)
          argslen += 1 + IntPtr.Size + s.Length;

        IntPtr args_ptr = csml_malloc(argslen);

        IntPtr i = args_ptr;
        IntPtr j = (IntPtr) ((long) args_ptr + IntPtr.Size + real_args.Length * IntPtr.Size);
        foreach (string s in real_args) {
          Marshal.WriteIntPtr(i, j);
          i = (IntPtr) ((long) i + IntPtr.Size);

          int len = s.Length;
          for (int k = 0; k < len; k ++) {
            Marshal.WriteByte(j, (byte)s[k]);
            j = (IntPtr) ((long) j + 1);
          }
          Marshal.WriteByte(j, 0);
          j = (IntPtr) ((long) j + 1);
        }
        Marshal.WriteIntPtr(i, IntPtr.Zero);
        csml_init(cs2ml_fptr_release_cs_value, cs2ml_fptr_resolve_cs2ml, args_ptr);
      }
      LexiFi.Interop.CsmlStd.Init();
    }

    private static Arrow<string, IntPtr> CS2MLResolver = delegate { return IntPtr.Zero; };
    public static void RegisterCS2MLResolver(Arrow<string, IntPtr> f) {
      Arrow<string, IntPtr> old = CS2MLResolver;
      CS2MLResolver = delegate (string s) {
        IntPtr r = f(s);
        if (r == IntPtr.Zero) return old(s); else return r;
      };
    }

    // This part is specific to LexiFi internal date type.

    static DateTime min_date = new DateTime(1980, 01, 01, 12, 0, 0);
    static DateTime max_date = new DateTime(2299, 12, 31, 23, 59, 0);
    public static DateTime csml_of_mldate(IntPtr v) {
      return min_date.AddMinutes (((uint) v >> 1) - 3600);
    }
    public static IntPtr csml_to_mldate(DateTime t) {
      // A tick is 100 nanoseconds. There are 6.10^8 ticks in a minute.
      // TODO: raise exception when not between min_date .. max_date
      if (t < min_date) t = min_date;
      else if (t > max_date) t = max_date;
      return (IntPtr) (((uint) ((t.Ticks - min_date.Ticks) / 600000000 + 3600) << 1) + 1);
    }
  }
}
