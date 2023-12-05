(* Inserts casts to L23Proj terms according to the Cast Insertion judgements defined by Siek and Taha *)
structure Cast : sig

  val cast : L23Proj.term -> Casted.term
  val castenv : TypeEnv.env * L23Proj.term -> Casted.term * Type.typ

end = struct

  structure L = L23Proj
  structure C = Casted
  structure T = Type
  structure E = TypeEnv

  fun castenv (gamma, L.Const(n)) = (C.Const(n), T.Int)
    | castenv (gamma, L.True) = (C.True, T.Bool)
    | castenv (gamma, L.False) = (C.False, T.Bool)
    | castenv (gamma, L.Var(x))
      = (case E.lookup(gamma, x)
          of SOME(tau) => (C.Var(x), tau)
           | NONE => raise Fail "could not cast variable")
    | castenv (gamma, L.Lam(x, sigma, e))
      = (case castenv(E.extend(gamma, x, sigma), e)
          of (e', tau) => (C.Lam(x, sigma, e'), T.Function(sigma, tau)))
    | castenv (gamma, L.ULam(x, e)) = castenv(gamma, L.Lam(x, T.Dynamic, e))
    | castenv (gamma, L.App(e1, e2))
      = (case (castenv(gamma, e1), castenv(gamma, e2))
          of ((e1', T.Dynamic), (e2', tau2))
              => (C.App(C.CastExp(T.Function(tau2, T.Dynamic), e1'), e2'), T.Dynamic)
           | ((e1', T.Function(tau, tau')), (e2', tau2))
              => if tau2 <> tau andalso Consistent.consistent(tau, tau2)
                 then (C.App(e1', C.CastExp(tau, e2')), tau')
                 else if tau2 = tau
                      then (C.App(e1', e2'), tau')
                      else raise Fail "could not cast application"
           | _ => raise Fail "must apply function or dynamic type")

  fun cast term 
    = (case castenv(E.empty, term)
        of (castedterm, tau) => castedterm)

end