structure Type = struct

  datatype typ
    = Int
    | Bool
    | Dynamic
    | Function of typ * typ

  val catw = String.concatWith
  fun tos Int = "I"
    | tos Bool = "B"
    | tos Dynamic = "?"
    | tos (Function (tau1, tau2)) = "(" ^ tos tau1 ^ " -> " ^ tos tau2 ^ ")"
  and par s = "(" ^ s ^ ")"

end
