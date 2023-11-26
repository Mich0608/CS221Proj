structure L23Proj = struct

  datatype term
    = Const of int
    | True
    | False
    | Var of string
    | ULam of string * term
    | Lam of string * Type.typ * term
    | App of term * term

  infix +-+
  fun s1 +-+ s2 = s1 ^ " " ^ s2
	       
  fun tos (Const n) = Int.toString n
    | tos True = "T"
    | tos False = "F"
    | tos (Var x) = x
    | tos (Lam (x, tau, t1)) = brack ("lam" +-+ x +-+ Type.tos tau +-+ tos t1)
    | tos (ULam (x, t1)) = brack ("lam" +-+ x +-+ tos t1)
    | tos (App (t1, t2)) = par (tos t1 +-+ tos t2)
  and par s   = "(" ^ s ^ ")"
  and brack s = "[" ^ s ^ "]"
  and brace s = "{" ^ s ^ "}"
			    
end

