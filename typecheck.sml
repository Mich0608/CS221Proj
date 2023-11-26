structure TypeCheck : sig

  val consistent : Type.typ * Type.typ -> bool

  val envtype : TypeEnv.env * L23Proj.term -> Type.typ
  val typeof : L23Proj.term -> Type.typ
							
end = struct

  structure L = L23Proj
  structure T = Type
  structure E = TypeEnv

  fun consistent (T.Function(t1, t2), T.Function(t3, t4))
    = consistent(t3, t1) andalso consistent(t2, t4)
    | consistent (t1, T.Dynamic) = true
    | consistent (T.Dynamic, t2) = true
    | consistent (t1, t2) = t1 = t2
  
  fun envtype (gamma, (L.Const(n))) = T.Int
    | envtype (gamma, L.True) = T.Bool
    | envtype (gamma, L.False) = T.Bool
    | envtype (gamma, L.Var(s))
      = (case E.lookup(gamma, s)
          of SOME(t) => t
           | NONE => raise Fail "invalid type: var")
    | envtype (gamma, L.Lam(x, tau, e))
      = T.Function(tau, envtype(E.extend(gamma, x, tau), e))
    | envtype (gamma, L.ULam(x, e))
      = T.Function(T.Dynamic, envtype(E.extend(gamma, x, T.Dynamic), e))
    | envtype (gamma, L.App(t1, t2))
      = (case (envtype(gamma, t1), envtype(gamma, t2))
          of (T.Function(tau, tau'), tau2) 
            => if consistent (tau2, tau)
               then tau'
               else raise Fail "invalid type: app argument not consistent with function argument type"
           | (T.Dynamic, tau2) => T.Dynamic
           | _ => raise Fail "invalid type: must apply function or dynamic type")

  fun typeof term = envtype(E.empty, term)
	    
end
