structure Test = struct

  (* Type Checking Tests *)

  fun typ_ term = Type.tos(TypeCheck.typeof(term))
  
  fun typ () = 
    let
      val _ = Check.expect(typ_ (L23Proj.True), "B", "typ0")
      val _ = Check.expect(typ_ (L23Proj.Const(0)), "I", "typ0")
      val _ = Check.expect(typ_ (L23Proj.ULam("x", L23Proj.Var("x"))), "(? -> ?)", "typ1")
      val _ = Check.expect(typ_ (L23Proj.Lam("x", Type.Bool, L23Proj.Var("x"))), "(B -> B)", "typ2")
      val _ = Check.expect(typ_ (L23Proj.Lam("x", Type.Dynamic, L23Proj.App(L23Proj.Lam("y", Type.Bool, L23Proj.Var("y")), L23Proj.Var("x")))), "(? -> B)", "typ3")
    in
      TextIO.print "typ tests done\n"
    end

  (* Cast Tests from Section 5.3 in Siek and Taha *)

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

    fun all () =
    let
      val _ = typ ()
      val _ = cast ()
      val _ = castedtyp ()
    in
      TextIO.print "all tests done\n"
    end

end