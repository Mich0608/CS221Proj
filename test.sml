structure Test = struct

  (* Cast Tests *)

  val castexample1 = L23Proj.App(L23Proj.ULam("x", L23Proj.App(L23Proj.Lam("s", Type.Int, L23Proj.Var("s")), L23Proj.Var("x"))), L23Proj.True)

  val castexpected1 = "([lam x:? ([lam s:I s] <I>x)] <?>T)"

  val castexample2 = L23Proj.App(L23Proj.Lam("f", Type.Function(Type.Dynamic, Type.Int), L23Proj.App(L23Proj.Var("f"), L23Proj.Const(1))), L23Proj.Lam("x", Type.Int, L23Proj.App(L23Proj.Lam("s", Type.Int, L23Proj.Var("s")), L23Proj.Var("x"))))

  val castexpected2 = "([lam f:(? -> I) (f <?>1)] <(? -> I)>[lam x:I ([lam s:I s] x)])"
  fun cast_ term = Casted.tos(Cast.cast(term))

  fun cast () =
    let
      val _ = Check.expect (cast_ castexample1, castexpected1, "cast1")
      val _ = Check.expect (cast_ castexample2, castexpected2, "cast2")
    in
      TextIO.print "cast tests done\n"
    end

  (* Casted Type Checking Tests *)

  fun castedtyp_ term = Type.tos(CastedTypeCheck.typeof(term))
  
  fun castedtyp () = 
    let
      val _ = Check.expect(castedtyp_ (Casted.CastExp(Type.Bool, Casted.True)), "B", "castedtyp0")
      val _ = Check.expect(castedtyp_ (Casted.Lam("x", Type.Dynamic, Casted.Var("x"))), "(? -> ?)", "castedtyp1")
      val _ = Check.expect(castedtyp_ (Casted.Lam ("x",Type.Dynamic,Casted.App (Casted.Lam ("y",Type.Bool,Casted.Var("y")),Casted.CastExp (Type.Bool,Casted.Var("x"))))), "(? -> B)", "castedtyp2")
      val _ = Check.exn (fn () => castedtyp_ (Casted.CastExp(Type.Bool, (Casted.Const(0)))), "badcastedtyp0")
    in
      TextIO.print "cast typ tests done\n"
    end

end