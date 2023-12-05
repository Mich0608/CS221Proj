structure CastedTypeCheck : sig

  val envtype : TypeEnv.env * Casted.term -> Type.typ
  val typeof : Casted.term -> Type.typ
							
end = struct

  structure C = Casted
  structure T = Type
  structure E = TypeEnv
  
  fun envtype (gamma, (C.Const(n))) = T.Int
    | envtype (gamma, C.True) = T.Bool
    | envtype (gamma, C.False) = T.Bool
    | envtype (gamma, C.Var(s))
      = (case E.lookup(gamma, s)
          of SOME(t) => t
           | NONE => raise Fail "invalid type: var")
    | envtype (gamma, C.Lam(x, tau, e))
      = T.Function(tau, envtype(E.extend(gamma, x, tau), e))
    | envtype (gamma, C.App(t1, t2))
      = (case (envtype(gamma, t1), envtype(gamma, t2))
          of (T.Function(tau, tau'), tau2) 
            => if tau = tau2
               then tau'
               else raise Fail "invalid type: app argument does not match function argument type"
           | _ => raise Fail "invalid type: must apply function or dynamic type")
    | envtype (gamma, (C.CastExp(typ, t1)))
        = if Consistent.consistent (envtype(gamma, t1), typ)
          then typ
          else raise Fail "invalid type: cast inconsistent with term"

  fun typeof term = envtype(E.empty, term)
	    
end
