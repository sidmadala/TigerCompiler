structure Err = ErrorMsg
structure A = Absyn
structure E = Env
structure S = Symbol
structure T = Types

structure Semant =
struct

type venv = E.enventry S.table
type tenv = T.ty S.table

type expty = {exp: Translate.exp, ty: T.ty}

val loopLevel = ref 0
fun isInLoop() = !loopLevel > 0
fun incLoopLevel() = loopLevel := !loopLevel + 1
fun decLoopLevel() = loopLevel := !loopLevel - 1

fun isSubType(T.BOTTOM, _) = true
  | isSubType(_, UNIT) = true
  | isSubType(NIL, RECORD(_)) = true
  | isSubType(INT, INT) = true
  | isSubType(STRING, STRING) = true
  | isSubType(T.RECORD(_, u1), T.RECORD(_, u2)) = u1 = u2
  | isSubType(T.ARRAY(_, u1), T.ARRAY(_, u2)) = u1 = u2
  | isSubType(T.NAME(s1, _), T.NAME(s2, _)) = String.compare(S.name s1, S.name s2) = EQUAL 
  | isSubType(_, _) = false


fun checkTyOrder(t1, t2, "sub", pos, errorMsg) = if isSubType(t1, t2) then () else Err.error pos errorMsg
  | checkTyOrder(t1, t2, "super", pos, errorMsg) = checkTyOrder(t2, t1, "sub", pos ,errorMsg)
  | checkTyOrder(t1, t2, "eq", pos, errorMsg) = if isSubType(t1, t2) andalso isSubType(t2, t1) then () else Err.error pos errorMsg
  | checkTyOrder(t1, t2, "assign", pos, errorMsg) = checkTyOrder(t1, t2, "super", pos, errorMsg)
  | checkTyOrder(T.INT, T.INT, "comp", pos, errorMsg) = ()
  | checkTyOrder(T.STRING, T.STRING, "comp", pos, errorMsg) = ()
  | checkTyOrder(T.INT, T.INT, "arith", pos, errorMsg) = ()
  | checkTyOrder(_, _, _, pos, errorMsg) = Err.error pos errorMsg


(* helper functions *)
fun getType(SOME(ty)) = ty
    | getType(NONE) = T.BOTTOM

fun actualTy(tenv, ty) = 
    case ty of
        T.NAME(name, tyref) => actualTy(tenv, getType(S.look(tenv, name)))
        | someTy => someTy

fun typeExtractor(tenv, T.NAME(sym, uniqueOpt), pos) = 
      let 
        fun extractorHelper(SOME(typ)) = typeExtractor(tenv, typ, pos)
          | extractorHelper(NONE) = (Err.error pos "error: symbol not defined"; T.BOTTOM) 
      in
        extractorHelper(!uniqueOpt)
      end
 | typeExtractor(tenv, typ : T.ty, pos) = typ

fun getTyFromSymbol(tenv, sym, pos) = 
      case S.look(tenv, sym) of 
        SOME(typ) => typeExtractor(tenv, typ, pos)
        | NONE => (ErrorMsg.error pos ("type not yet defined"); T.INT)
(*
fun checkInt({exp=_, ty=T.INT}, pos) = ()
    | checkInt ({exp=_, ty=_ }, pos) = Err.error pos "error: not an integer"

fun checkTyComp({exp=_, ty = T.INT}, {exp=_, ty = T.INT}, pos) = ()
  | checkTyComp({exp=_, ty = T.STRING}, {exp=_, ty = T.STRING}, pos) = ()
  | checkTyComp(_, _, pos) = Err.error pos "error: not comparable"

fun checkTyEq({exp=_, ty = T.INT}, {exp, ty = T.INT}, pos) = ()
  | checkTyEq({exp=_, ty = T.STRING}, {exp, ty = T.STRING}, pos) = ()
  | checkTyEq({exp=_, ty = T.UNIT}, {exp, ty = T.UNIT}, pos) = ()
  | checkTyEq({exp=_, ty = T.RECORD(_)}, {exp, ty = T.NIL}, pos) = ()
  | checkTyEq({exp=_, ty = T.NIL}, {exp, ty = T.RECORD(_)}, pos) = ()
  | checkTyEq({exp=_, ty = T.RECORD(_, u1)}, {exp, ty = T.RECORD(_, u2)}, pos) =
    if u1 = u2 then () else Err.error pos "error: record types mismatch"
  | checkTyEq({exp=_, ty = T.ARRAY(_, u1)}, {exp, ty = T.ARRAY(_, u2)}, pos) =
    if u1 = u2 then () else Err.error pos "error: array types mismatch"
  | checkTyEq(_, _, pos) = Err.error pos "error: types not equal"

fun isSameType(tenv, T.UNIT, T.UNIT, pos : Absyn.pos) = true
  | isSameType(tenv, T.INT, T.INT, pos) = true
  | isSameType(tenv, T.STRING, T.STRING, pos) = true
  | isSameType(tenv, T.RECORD(_, u1), T.RECORD(_, u2), pos) = (u1 = u2)
  | isSameType(tenv, T.RECORD(_), T.UNIT, pos) = true
  | isSameType(tenv, T.UNIT, T.RECORD(_), pos) = true
  | isSameType(tenv, T.ARRAY(_, u1), T.ARRAY(_, u2), pos) = (u1 = u2)
  | isSameType(tenv, T.NAME(s1, _), T.NAME(s2, _), pos) = String.compare(S.name
  s1, S.name s2) = EQUAL
  | isSameType(tenv, ty1, ty2, pos) = false
*)
(* beginning of main transExp function *)
fun transExp(venv, tenv, exp) =
    let
      fun trexp(A.NilExp) = {exp = (), ty = Types.NIL}
        | trexp(A.IntExp(i)) = {exp = (), ty = Types.INT}
        | trexp(A.StringExp(s, pos)) = {exp = (), ty = Types.STRING}
        | trexp(A.VarExp(var)) = trvar(var)

        (* SeqExp 🐶*)  
        | trexp(A.SeqExp([])) = {exp = (), ty = Types.UNIT}
        | trexp(A.SeqExp([(exp, pos)])) = trexp(exp)
        | trexp(A.SeqExp((exp, pos)::l)) = (trexp(exp); trexp(A.SeqExp l))

        (* OpExp: check for type equality 🐶*)
        | trexp(A.OpExp{left, oper, right, pos}) = 
          (case oper of 
              A.PlusOp =>   (checkTyOrder(#ty (trexp left), #ty (trexp right), "arith", pos, "not an integer"); {exp=(), ty=T.INT})
            | A.MinusOp =>  (checkTyOrder(#ty (trexp left), #ty (trexp right), "arith", pos, "not an integer"); {exp=(), ty=T.INT})
            | A.TimesOp =>  (checkTyOrder(#ty (trexp left), #ty (trexp right), "arith", pos, "not an integer"); {exp=(), ty=T.INT})
            | A.DivideOp => (checkTyOrder(#ty (trexp left), #ty (trexp right), "arith", pos, "not an integer"); {exp=(), ty=T.INT})
            | A.EqOp =>  (checkTyOrder(#ty (trexp left), #ty (trexp right), "eq", pos, "types mismatch"); {exp=(), ty=T.INT})
            | A.NeqOp => (checkTyOrder(#ty (trexp left), #ty (trexp right), "eq", pos, "types mismatch"); {exp=(), ty=T.INT})
            | A.LtOp => (checkTyOrder(#ty (trexp left), #ty (trexp right), "comp", pos, "not comparable"); {exp=(), ty=T.INT})
            | A.LeOp => (checkTyOrder(#ty (trexp left), #ty (trexp right), "comp", pos, "not comparable"); {exp=(), ty=T.INT})
            | A.GtOp => (checkTyOrder(#ty (trexp left), #ty (trexp right), "comp", pos, "not comparable"); {exp=(), ty=T.INT})
            | A.GeOp => (checkTyOrder(#ty (trexp left), #ty (trexp right), "comp", pos, "not comparable"); {exp=(), ty=T.INT})
          )

        (* LetExp 🐶*) 
        | trexp(A.LetExp{decs, body, pos}) = 
            let 
              val {venv = venv', tenv = tenv'} = transDec(venv, tenv, decs)
            in
              transExp(venv', tenv', body)
            end

        (* BreakExp 🐶*) 
        | trexp(A.BreakExp(pos)) = (
          if not isInLoop() then Err.error pos "illegal break" else (); 
          {exp = (), ty = T.UNIT}
          ) 

        (* WhileExp 🐶*) 
        | trexp(A.WhileExp{test, body, pos}) = (
          checkTyOrder(#ty (trexp test), T.INT, "eq", pos, "not an integer");
          incLoopLevel(); 
          checkTyOrder(#ty (trexp body), T.UNIT, "eq", pos, "while loop should return UNIT"); 
          decLoopLevel(); 
          {exp = (), ty = T.UNIT}
          )

        (* ForExp 🐶*) 
        | trexp(A.ForExp{var, escape, lo, hi, body, pos}) = (
          checkTyOrder(#ty (trexp lo), T.INT, "eq", pos, "not an integer");
          checkTyOrder(#ty (trexp hi), T.INT, "eq", pos, "not an integer");
          incLoopLevel(); 
          checkTyOrder(#ty (transExp(S.enter(venv, var, E.VarEntry{ty=T.INT}), tenv, body)), T.UNIT, "eq", pos, "for loop should return UNIT"); 
          decLoopLevel(); 
          {exp = (), ty = T.UNIT}
          ) 

        (* IfExp 🐶*) 
        | trexp(A.IfExp{test, then', else', pos}) = (
          checkTyOrder(#ty (trexp test), T.INT, "eq", pos, "not an integer");
          if isSome(else')
          then (
            checkTyOrder(#ty (trexp then'), #ty (trexp (valOf(else'))), "eq", pos, "then and else should return the same type");
            {exp = (), ty = #ty (trexp then')}
          )
          else (
            checkTyOrder(#ty (trexp then'), T.UNIT, "eq", pos, "then should return UNIT");
            {exp = (), ty = T.UNIT})
          )

        (* AssignExp: TODO: implement for loop var unassignable*)  
        | trexp(A.AssignExp{var, exp, pos}) = (
          checkTyOrder(#ty (trvar(var)), #ty (trexp(exp)), "assign", pos, "error: var and exp types don't match");
          {exp = () , ty = T.UNIT} 
          )

        (* CallExp 🐶*)
        | trexp(A.CallExp{func, args, pos}) = 
        (* 1. S.look if function exists
        2. check if argument typing works out  *)
          (let 
          (* TODO: "eq" or "super" *)
          fun checkFunParams(f::formals, a::args, pos) = checkTyOrder(f, #ty (trexp a), "super", pos, "error: argument mismatch")
            | checkFunParams([], a::args, pos) = (Err.error pos "error: too many arguments given"; ())
            | checkFunParams(f::formals, [], pos) = (Err.error pos "error: not enough arguments given"; ())
            | checkFunParams([], [], pos) = ()
          in 
            case S.look(venv, func) of
              SOME(Env.FunEntry({formals, result})) => (checkFunParams(formals, args, pos); {exp=(), ty=result})
              | SOME(_) => (Err.error pos "error: not a function"; {exp=(), ty=T.UNIT})
              | NONE => (Err.error pos "error: function not declared"; {exp=(), ty=T.UNIT})
          end
          )

        (* ArrayExp  TODO: super? *)
        | trexp(A.ArrayExp({typ, size, init, pos})) =
          (case S.look(tenv, typ) of
              SOME x =>
                (case actualTy x of
                  T.ARRAY(ty, unique) => (
                    checkTyOrder(#ty (trexp size), T.INT, "eq", pos, "not an integer");
                    checkTyOrder(actualTy ty, #ty (trexp init), "super", pos, "error: array type and initializing exp differ");
                    {exp=(), ty=T.ARRAY(ty, unique)}
                  )
                  | _ => (Err.error pos "type not an array"; {exp=(), ty=T.BOTTOM})
                )
            | NONE => (Err.error pos "No such type"; {exp=(), ty=T.BOTTOM})
          )
          
        (* RecordExp TODO: actual? *)
        | trexp(A.RecordExp({fields, typ, pos})) = 
            (case S.look(tenv, typ) of
                  SOME x => 
                      (case actualTy x of
                          T.RECORD(f, _) => 
                                let 
                                    fun check(({fieldSym, fieldExp, pos}, {recFormalSym, recFormalTy}), ()) = 
                                      (
                                        if String.compare(S.name fieldSym, S.name recFormalSym) <> EQUAL
                                        then Err.error pos ("field name doesn't match")
                                        else ();
                                        checkTyOrder(recFormalTy, #ty (trexp fieldExp), "super", pos, "field type doesn't match");
                                      )
                                in
                                    if List.length(recFormal) <> List.length(fields)
                                    then (Err.error pos ("record list is wrong length: " ^ S.name typ); {exp=(), ty= T.NIL})
                                    else (foldr check () ListPair.zip(fields, f()); {exp=(), ty= actualTy x})
                                end
                          | _ => (Err.error pos ("error : expected record type, not: " ^ S.name typ); {exp=(), ty=T.NIL})
                        )
                  | NONE => (Err.error pos ("error : invalid record type: " ^ S.name typ); {exp=(), ty=T.NIL})
                )

          (* SimpleVar  🐶*)
      and trvar(A.SimpleVar(sym, pos)) =
        (case S.look(venv, sym) of
              SOME(Env.VarEntry({ty})) => {exp=(), ty= ty} 
            | SOME(Env.FunEntry({formals, result})) => {exp=(), ty= result} 
            | NONE => (Err.error pos ("error: variable not declared" ^ S.name sym); {exp=(), ty= T.BOTTOM})
        )
          (* FieldVar *)
        | trvar(A.FieldVar(var, sym, pos)) = 
            (case trvar var of 
                {exp=(), ty=T.RECORD(fieldTys, unique)} =>
                    let
                      val fields = fieldTys()
                      fun getFieldType((fieldSym, fieldTy)::l, id : Absyn.symbol, pos : Absyn.pos) =
                            if String.compare(S.name fieldSym, S.name sym) = EQUAL 
                            then 
                              case S.look(tenv, fieldSym) of
                                SOME(ty) => ty
                                |NONE => (Err.error pos ("error: type does not exist in fields"); T.BOTTOM)
                            else getFieldType(l, id, pos)
                        | getFieldType([], id, pos) = (Err.error pos ("error: field does not exist in record"); T.BOTTOM)
                    in
                      {exp=(), ty=getFieldType(fields, sym, pos)}
                    end
                | {exp=_, ty=_} => (Err.error pos ("error: not a record"); {exp=(), ty=T.BOTTOM})
            )
            (* SubscriptVar *)
        | trvar(A.SubscriptVar(var, exp, pos)) =
              (case trvar var of
                {exp=(), ty=T.ARRAY(arrTy, unique)} => (checkInt(trexp exp, pos); {exp=(), ty=actualTy(tenv, arrTy)})
                | {exp=_, ty=_} => (Err.error pos ("error: not an array"); {exp=(), ty=T.BOTTOM})  
              )    
    in
      trexp(exp)
    end

and transDec(venv, tenv, decs) = 
        let 
          (* VarDec 🐶 *)
          fun trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos})) =
              (
                val initTy = #ty (transExp(venv, tenv, init));
                case typ of
                    SOME(symbol, pos) =>
                        (case S.look(tenv, symbol) of
                            SOME ty => (checkTyOrder(actualTy(tenv, ty), initTy, "super", pos, "types mismatch");
                                       {venv=S.enter(venv, name, (Env.VarEntry{ty = actualTy(tenv, ty)})), tenv = tenv})
                          | NONE => (Err.error pos "type not recognized"; {venv = venv, tenv = tenv})
                        )
                  | NONE =>
                        (checkTyOrder(initTy, T.NIL, "eq", pos, "error: initializing nil expressions not constrained by record type");
                        {venv=S.enter(venv, name, (Env.VarEntry{ty = initTy})), tenv = tenv}
                        )        
                )
          (* TypeDec  TODO: should we break the cycle here or add extra stuff in actualTy to avoid inifite loop?*)
          | trdec(venv, tenv, A.TypeDec(tydeclist)) =
              let
                val tenvDummyTy = foldl (fn({name, ty, pos}, ans) => S.enter(ans, name, T.BOTTOM)) tenv tydeclist
                val tenvActualTy = foldl (fn({name, ty, pos}, ans) => S.enter(ans, name, transTy(tenvDummyTy, ty))) tenv tydeclist
                fun checkIllegalCycle({name, ty, pos}, ()) = 
                  let
                    fun checkHelper(seenList, name) =
                      (
                      case S.look(tenvActualTy, name) of
                           SOME(T.NAME(symb, _)) => if List.exists (fn s => String.compare(s, S.name symb) = EQUAL) seenList
                                                    then Err.error pos "error: mutually recursive types thet do not pass through record or array - cycle"
                                                    else checkHelper((S.name name)::seenList, symb)
                         | _ => ()
                      )
                  in
                    checkHelper([], name)
                  end
              in
                foldl (fn({name, ty, pos}, l) => if List.exists (fn s => String.compare(s, S.name name) = EQUAL) l then (Err.error pos "error : two types of same name in mutually recursive tydec"; l) else (S.name name :: l))  [] tydeclist;
                foldl checkIllegalCycle () tydeclist;
                {tenv = tenvActualTy, venv = venv}
              )
             (* FunctionDec  TODO: use actualTy?? *)
          | trdec(venv, tenv, A.FunctionDec(fundeclist)) =
                let 
                    fun transrt rt =
                        (case S.look(tenv, rt) of 
                            SOME(ty) => actualTy(tenv, ty)
                          | NONE => (Err.error 0 ("Return type unrecognized: " ^ S.name rt); T.BOTTOM)
                        )
                    fun transparam {name, escape, typ, pos} = 
                        (case S.look(tenv, typ) of
                            SOME ty => {name = name, ty = actualTy(tenv, ty)}
                          | NONE => (Err.error 0 ("Parameter type unrecognized: " ^ S.name typ); T.BOTTOM)
                        )
                    fun enterFuncs ({name, params, body, pos, result=SOME(rt, pos')}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=transrt rt})
                      | enterFuncs ({name, params, body, pos, result=NONE}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=T.UNIT})
                    val venv' = foldl enterFuncs venv fundeclist
                    fun checkfundec({name, params, body, pos, result}) = 
                        let 
                            val resultTy = 
                                (case result of
                                    SOME(rt, pos') => transrt rt
                                  | NONE => T.UNIT
                                )
                            val venv'' = foldl (fn({name, ty}, ans) => S.enter(ans, name, Env.VarEntry{ty=ty})) venv' (map transparam params)
                            val bodyResult = transExp (venv'', tenv, body)
                        in
                          checkTyOrder(#ty bodyResult, resultTy, "sub", pos, "Function body type doesn't match return type in function " ^ S.name name)
                        end 
                    fun foldfundec (fundec, ()) = checkfundec fundec
                in
                    foldl (fn({name, params, body, pos, result}, l) => if List.exists (fn s => String.compare(s, S.name name) = EQUAL) l then (Err.error pos "error : two types of same name in mutually recursive fundec"; l) else (S.name name :: l))  [] fundeclist;
                    foldr foldfundec () fundeclist;
                    {venv=venv', tenv=tenv}
                end
            and folddec(dec, {venv, tenv}) = trdec(venv, tenv, dec)
        in
            foldl folddec {venv=venv, tenv=tenv} decs
        end
        
and transTy(tenv, ty) =
  let 
    (* NameTy 🐶  
    NOTE: even if the result is not a dummy type we still store the mapping as a dummy type,
    so we need actualTy to translate the dummy type when we wanna use it
    e.g.
    type a = b
    type b = int
    a => NAME(b)  
    b => int
   ---------------------
    type b = int
    type a = b
    a => NAME(b)
    b => int
    Like in the second example we could have stored a => int, but we chose not to do that
    *)
    fun trty(tenv, A.NameTy (name, _)) = 
      (case S.look(tenv, name) of
        SOME _ => T.NAME(name, ref(NONE))
      | NONE => (Err.error 0 ("error: unrecognized name type: " ^ S.name name); T.NAME(name, ref(NONE)))
      )    
    (* RecordTy *)
    | trty(tenv, A.RecordTy(fields)) = 
        let 
          fun trfields {name, escape, typ, pos} =
            case S.look(tenv, typ) of
              SOME(ty) => (name, ty)
            | NONE => (Err.error pos ("error: undefined type in record field: " ^ S.name typ); (name, T.NIL))  
            (* TODO: fix one of these things idk what's wrong @MICHELLE *)
            fun recGen() = foldl (fn (a, b) => trfields(a)::b) [] fields
        in 
            T.RECORD (recGen, ref ())
        end
      (* ArrayTy 🐶*)
    | trty(tenv, A.ArrayTy(sym, pos)) = 
        T.ARRAY (transTy (tenv, A.NameTy (sym, pos)), ref ()) (* FIX: may need to call less scoped function *)
  in
    trty(tenv, ty)
  end

(* transProg needs to take in expression to translate, run transExp, and return unit *)
fun transProg(exp_to_translate : A.exp) = 
    (transExp (E.base_venv, E.base_tenv, exp_to_translate); ())

end 
