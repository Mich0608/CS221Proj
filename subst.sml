structure Subst : sig

  val subst : string * L23Proj.term * L23Proj.term -> L23Proj.term
	    
end = struct

  structure L = L23Proj

(* note: We will do without VarSet/FV this time. *)
(* This is because free variables are effectively banished by the typechecker. *)
(* That is, we shouldn't have them when we get to evaluation. *)
		  
  fun subst (x, s, t)
    = (case t
       of L.Var y => if x=y then s else t
        | L.App (tA, tB) => L.App (subst (x, s, tA), subst (x, s, tB))
        | L.Lam (y, tau, tB) =>
            if x = y then L.Lam (y, tau, tB)
            else L.Lam (y, tau, subst (x, s, tB))
        | L.ULam (y, tB) =>
            if x = y then L.ULam (y, tB)
            else L.ULam (y, subst (x, s, tB))
        | L.Const(n) => L.Const(n)
        | L.True => L.True
        | L.False => L.False)

end
