// The class Counter is mostly produced by the csml compiler. Since it is partial, it is possible to add
// extra components or declare that it implements some interface (or inherit from some class).

public interface ICounter {
  int Linear(int i);
}

public partial class Counter : ICounter {
}

public static class OpaqueBinding {
  public static void Main() {
    LexiFi.Interop.Csml.Init();
    LexiFi.Interop.OpaqueBinding.Init();
    /* Support for addin mode. */
    if (!LexiFi.Interop.Csml.MLStubAvailable("OpaqueBinding"))
        LexiFi.Interop.Csml.LoadFile("opaque_binding.cma");

    System.Console.WriteLine("Version = " + Counter.Version);

    Counter c = new Counter();
    System.Console.WriteLine("c: " + c);
    c.Bar = 3;
    c.Foo = 10;
    System.Console.WriteLine("c: " + c);
    System.Console.WriteLine(c.Linear(2));

    Counter.Test();
  }
}



// We define a class to be mapped opaquely to an abstract ML type

public class MyClass {
  private int i = 0;
  public MyClass() { }
  public MyClass(int i) { this.i = i; }
  public int Value { get { return i; }   set { i = value; } }
  public void Bump() { i++; }
  public void Bump(int x) { i += x; }
}
