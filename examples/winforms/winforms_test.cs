public static partial class App {
  [System.STAThread]
  public static void Main() {
    LexiFi.Interop.WinForms.Init();
    LexiFi.Interop.WinFormsTest.Init();

    /* Support for addin mode. */
    if (!LexiFi.Interop.Csml.MLStubAvailable("WinFormsTest"))
        LexiFi.Interop.Csml.LoadFile("winforms.cma");

    Run(); // This is the method specified in winform_test.csml, calling the OCaml function Winform_test.run
  }
}
