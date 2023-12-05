structure Consistent : sig

  val consistent : Type.typ * Type.typ -> bool
	    
end = struct

  structure T = Type

  fun consistent (T.Function(t1, t2), T.Function(t3, t4))
    = consistent(t3, t1) andalso consistent(t2, t4)
    | consistent (t1, T.Dynamic) = true
    | consistent (T.Dynamic, t2) = true
    | consistent (t1, t2) = t1 = t2
					  
end
