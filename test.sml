structure Test = struct

  fun cast_ term = Casted.tos(Cast.cast(term))

  val example1 = L23Proj.App(L23Proj.ULam("x", L23Proj.App(L23Proj.Lam("s", Type.Int, L23Proj.Var("s")), L23Proj.Var("x"))), L23Proj.True)

  val expected1 = "([lam x:? ([lam s:I s] <I>x)] <?>T)"

  val example2 = L23Proj.App(L23Proj.Lam("f", Type.Function(Type.Dynamic, Type.Int), L23Proj.App(L23Proj.Var("f"), L23Proj.Const(1))), L23Proj.Lam("x", Type.Int, L23Proj.App(L23Proj.Lam("s", Type.Int, L23Proj.Var("s")), L23Proj.Var("x"))))

  val expected2 = "([lam f:(? -> I) (f <?>1)] <(? -> I)>[lam x:I ([lam s:I s] x)])"
      
  fun cast () =
    let
      val _ = Check.expect (cast_ example1, expected1, "cast1")
      val _ = Check.expect (cast_ example2, expected2, "cast2")
    in
      TextIO.print "cast tests done\n"
    end
      
end
