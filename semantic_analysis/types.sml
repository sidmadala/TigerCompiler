structure Types =
struct
  type unique = unit ref

  datatype ty = 
            RECORD of (unit -> (Symbol.symbol * ty) list) * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	  | NAME of Symbol.symbol * ty option ref
	  | UNIT
    | BOTTOM

  (* Types returned by comparison in lattice *)
  datatype comp = 
    LT
  | GT
  | EQ
  | INCOMP (* incomparable *)

  (* Type lattice for comparison operator *)
  fun leq(BOTTOM, _) = true
    | leq(_, UNIT) = true
    | leq(NIL, RECORD(_)) = true
    | leq(INT, INT) = true
    | leq(STRING, STRING) = true
    | leq(RECORD(_, unique1), RECORD(_, unique2)) = (unique1 = unique2)
    | leq(ARRAY(_, unique1), ARRAY(_, unique2)) = (unique1 = unique2)
    | leq(NIL, NIL) = true
    | leq(NAME(sym1, _), NAME(sym2, _)) = String.compare(Symbol.name sym1, Symbol.name sym2) = EQUAL
    | leq(_, _) = false

  (* Comparison operator for types *)
  fun comp(t1, t2) = 
    if leq(t1, t2) andalso leq(t2, t1)
      then EQ
    else if leq(t1, t2)
      then LT
    else if leq(t2, t1)
      then GT
    else
      INCOMP

  (* For debugging what type we are currently looking at *)
  fun printTy ty =
    case ty of
          RECORD(_, _) => print "type is record\n"
        | NIL => print "type is nil\n"
        | INT => print "type is int\n"
        | STRING => print "type is string\n"
        | ARRAY(arrTy, _) => (print "array: "; printTy ty)
        | NAME(sym, _) => print ("name type is " ^ Symbol.name sym ^ "\n")
        | UNIT => print "type is unit\n"
        | BOTTOM => print "type is bottom\n"

end

