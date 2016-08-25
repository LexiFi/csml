using System.Collections.Generic;
using LexiFi.Interop;

public static class StructuredMapping {
  public static void Main() {
    LexiFi.Interop.Csml.Init();
    LexiFi.Interop.StructuredMapping.Init();

    /* Support for addin mode. */
    if (!LexiFi.Interop.Csml.MLStubAvailable("StructuredMapping"))
        LexiFi.Interop.Csml.LoadFile("structured_mapping.cma");

    List<Expr> el = new List<Expr>();
    el.Add(new Expr.Var("x"));
    el.Add(new Expr.Var("y"));
    el.Add(Expr.Fraction(new Expr.Lit(1), new Expr.Lit(2)));
    Expr e = new Expr.Plus(el);

    List<Tuple<string,double>> env = new List<Tuple<string,double>>();
    env.Add(new Tuple<string, double>("x", 1));
    env.Add(new Tuple<string, double>("y", 2));
    double x = e.Eval(env);

    System.Console.WriteLine("result = " + x);
  }
}


public class VariantMatching : MyVariant.Match<int> {
  override public int A() { return 1; }
  override public int Bee(string s, int i) { return i; }
  override public int C(MyVariant x) { return this.run(x); }

  public static int Match(MyVariant x) {
    return (new VariantMatching()).run(x);
  }
}
