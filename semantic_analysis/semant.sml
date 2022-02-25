structure Err = ErrorMsg
structure A = Absyn
structure E = Env
structure S = Symbol

structure Semant =
struct

type venv = E.enventry S.table
type tenv = T.ty S.table

type expty = {exp: Translate.exp, ty: T.ty}

val loopLevel = ref 0
fun incLoopLevel() = loopLevel := !loopLevel + 1
fun decLoopLevel() = loopLevel := !loopLevel - 1

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

(* beginning of main transExp function *)
fun transExp(venv, tenv, exp) =
    let
      fun trexp(A.NilExp) = {exp = (), ty = Types.NIL}
        | trexp(A.IntExp(i)) = {exp = (), ty = Types.INT}
        | trexp(A.StringExp(s, pos)) = {exp = (), ty = Types.STRING}
        | trexp(A.VarExp(var)) = trvar(var)
        (* SeqExp *)
        | trexp(A.SeqExp([])) = {exp = (), ty = Types.UNIT}
        | trexp(A.SeqExp([(exp, pos)])) = trexp(exp)
        (* TODO: FIX BUG @zian *)
        | trexp(A.SeqExp((exp, pos)::l)) = (trexp(exp); trexp(A.SeqExp l))
        (* OpExp: check for type equality*)
        | trexp(A.OpExp{left, oper, right, pos}) = 
          (case oper of 
              A.PlusOp => (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp=(), ty=T.INT})
            | A.MinusOp => (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp=(), ty=T.INT})
            | A.TimesOp => (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp=(), ty=T.INT})
            | A.DivideOp => (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp=(), ty=T.INT})
            | A.EqOp => (checkTyEq(trexp left, trexp right, pos); {exp=(), ty=T.INT})
            | A.NeqOp => (checkTyEq(trexp left, trexp right, pos); {exp=(), ty=T.INT})
            | A.LtOp => (checkTyComp(trexp left, trexp right, pos); {exp=(), ty=T.INT})
            | A.LeOp => (checkTyComp(trexp left, trexp right, pos); {exp=(), ty=T.INT})
            | A.GtOp => (checkTyComp(trexp left, trexp right, pos); {exp=(), ty=T.INT})
            | A.GeOp => (checkTyComp(trexp left, trexp right, pos); {exp=(), ty=T.INT})
          )
        (* LetExp *)
        | trexp(A.LetExp{decs, body, pos}) = 
            let 
              val {venv = venv', tenv = tenv'} = transDec(venv, tenv, decs)
            in
              transExp(venv', tenv', body)
            end
        (* BreakExp *)
        | trexp(A.BreakExp(pos)) = (if !loopLevel = 0 then Err.error pos "illegal break" else (); {exp = (), ty = T.UNIT}) 
        (* WhileExp *)
        | trexp(A.WhileExp{test, body, pos}) = (checkInt(trexp test, pos);
        incLoopLevel(); if isSameType(tenv, #ty (trexp body), T.UNIT, pos) then () else Err.error pos "while loop should return UNIT"; decLoopLevel(); {exp = (), ty = T.UNIT})
        (* ForExp *)
        | trexp(A.ForExp{var, escape, lo, hi, body, pos}) = (checkInt(trexp lo, pos); checkInt(trexp hi, pos); incLoopLevel(); if isSameType(tenv,
        #ty (transExp(S.enter(venv, var, E.VarEntry{ty=T.INT}), tenv, body)), T.UNIT, pos) then () else Err.error pos "for loop should return UNIT"; decLoopLevel(); {exp = (), ty = T.UNIT}) 
        (* IfExp *)
        | trexp(A.IfExp{test, then', else', pos}) = (checkInt(trexp test, pos);
        if isSome(else')
        then 
            (* TODO: FIX TREXP VALOF @ZIAN *)
            (if isSameType(tenv, #ty (trexp then'), #ty (trexp (valOf(else'))), pos)
             then () else Err.error pos "then and else should return the same type";
             {exp = (), ty = #ty (trexp then')})
        else 
            (if isSameType(tenv, #ty (trexp then'), T.UNIT, pos) 
             then () else Err.error pos "then should return UNIT";
             {exp = (), ty = T.UNIT}))

        (* AssignExp *)
        | trexp(A.AssignExp{var, exp, pos}) = 
          (if isSameType(tenv, #ty (trvar(var)), #ty (trexp(exp)), pos)
              then () else Err.error pos "error: var and exp types don't match";
              {exp = () , ty = T.UNIT} 
              )
        (* CallExp *)
        | trexp(A.CallExp{func, args, pos}) = 
        (* 1. S.look if function exists
        2. check if argument typing works out  *)
          (let 
          fun getTypeFromExp({exp=_, ty=someTy}) = someTy
          fun checkFunParams(f::formals, a::args, pos) = 
                if isSameType(tenv, f, getTypeFromExp(trexp a), pos) 
                then checkFunParams(formals, args, pos) 
                else (Err.error pos "error: argument mismatch"; ())
              | checkFunParams([], a::args, pos) = (Err.error pos "error: too many arguments given"; ())
              | checkFunParams(f::formals, [], pos) = (Err.error pos "error: not enough arguments given"; ())
              | checkFunParams([], [], pos) = ()
          in 
            case S.look(venv, func) of
              SOME(Env.FunEntry({formals, result})) => (checkFunParams(formals, args, pos); {exp=(), ty=result})
              | SOME(_) => (Err.error pos "error: why this is not a function (does this happen? idk)"; {exp=(), ty=T.UNIT})
              | NONE => (Err.error pos "error: function not declared"; {exp=(), ty=T.UNIT})
          end
          )
        | trexp(A.ArrayExp({typ, size, init, pos})) =
            (checkInt(trexp size, pos);
            if isSameType(tenv, #ty (trexp(init)), getTyFromSymbol(tenv, typ, pos), pos)
            then ()
            else Err.error pos "error: array type and initializing exp differ";
            {exp=(), ty=T.ARRAY(#ty (trexp(init)), ref())}
            )
      and trvar(A.SimpleVar(sym, pos)) =
        (case S.look(venv, sym) of
              SOME(Env.VarEntry({ty})) => {exp=(), ty= ty} 
            | SOME(Env.FunEntry({formals, result})) => {exp=(), ty= result} 
            | NONE => (Err.error pos ("error: variable not declared" ^ S.name sym); {exp=(), ty= T.BOTTOM})
        )
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
        | trvar(A.SubscriptVar(var, exp, pos)) =
              (case trvar var of
                {exp=(), ty=T.ARRAY(arrTy, unique)} => (checkInt(trexp exp, pos); {exp=(), ty=actualTy(tenv, arrTy)})
                | {exp=_, ty=_} => (Err.error pos ("error: not an array"); {exp=(), ty=T.BOTTOM})  
              )    
    in
      trexp(exp)
    end

and transDec(venv, tenv, decs) = 
        let fun
            trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos})) =
                let
                  fun getType(SOME(ty)) = ty
                    | getType(NONE) = T.BOTTOM
                  fun actualTy ty = 
                    case ty of
                        T.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
                      | someTy => someTy
                in
                    (
                    case typ of
                        SOME(symbol, pos) =>
                            (case S.look(tenv, symbol) of
                                SOME ty => (checkTyEq({exp=(), ty = actualTy
                                ty}, {exp = (), ty = #ty
                                (transExp(venv, tenv, init))}, pos);
                                           {venv=S.enter(venv, name, (Env.VarEntry{ty=actualTy ty})), tenv=tenv})
                              | NONE => (Err.error pos "type not recognized"; {venv=venv, tenv=tenv})
                            )
                      | NONE =>
                            let 
                              val {exp, ty} = transExp(venv, tenv, init)
                            in 
                              if isSameType(tenv, ty, T.NIL, pos)
                              then Err.error pos "error: initializing nil expressions not constrained by record type"
                              else ();
                              {venv=S.enter(venv, name, (Env.VarEntry{ty=ty})), tenv=tenv}
                            end
                    )
                end
          | trdec(venv, tenv, A.TypeDec(tydeclist)) =
                let
                  fun maketemptydec ({name, ty, pos}, tenv') = S.enter(tenv', name, T.BOTTOM)
                  val temp_tenv = foldl maketemptydec tenv tydeclist
                  fun foldtydec({name, ty, pos}, {venv, tenv}) = {venv=venv, tenv=S.enter(tenv, name, transTy(temp_tenv, ty))}
                  val new_env = foldl foldtydec {venv=venv, tenv=tenv} tydeclist

                  fun checkIllegalCycle({name, ty, pos}, ()) = 
                  let
                    fun checkHelper(seenList, name) =
                      (
                      case S.look(#tenv new_env, name) of
                           SOME(T.NAME(symb, _)) => if List.exists (fn y => String.compare(S.name symb, S.name y) = EQUAL) seenList
                                                    then Err.error pos "error: mutually recursive types thet do not pass through record or array - cycle"
                                                    else checkHelper(name::seenList, symb)
                         | _ => ()
                      )
                  in
                    checkHelper([], name)
                  end

                  fun checkDuplicates({name, ty, pos}, seenList) = 
                      if List.exists (fn y => String.compare(S.name name, y) = EQUAL) seenList
                      then (Err.error pos "error : two types of same name in mutually recursive tydec"; seenList)
                      else (S.name name)::seenList

                in
                  foldl checkDuplicates [] tydeclist;
                  foldl checkIllegalCycle () tydeclist;
                  new_env
                end (*
          | trdec(venv, tenv, A.FunctionDec(fundeclist)) =
                let 
                    fun transrt rt =
                        (case S.look(tenv, rt) of 
                            SOME(rt') => rt'
                          | NONE => (Err.error 0 ("Return type unrecognized: " ^ S.name rt); T.BOTTOM)
                        )
                    fun transparam {name, escape, typ, pos} = 
                        (case S.look(tenv, typ) of
                            SOME t => {name=name, ty=t}
                          | NONE => (Err.error 0 ("Parameter type unrecognized: " ^ S.name typ); {name=name, ty=T.BOTTOM})
                        )
                    fun enterFuncs ({name, params, body, pos, result=SOME(rt, pos')}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=transrt rt})
                      | enterFuncs ({name, params, body, pos, result=NONE}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=T.UNIT})
                    val venv' = foldr enterFuncs venv fundeclist
                    fun checkfundec({name, params, body, pos, result}) = 
                        let 
                            val result_ty = 
                                (case result of
                                    SOME(rt, pos') => transrt rt
                                  | NONE => T.UNIT
                                )
                            val params' = map transparam params
                            fun enterparam ({name, ty}, venv) = S.enter(venv, name, Env.VarEntry{ty=ty})
                            val venv'' = foldl enterparam venv' params'
                            val body' = transExp (venv'', tenv, body)
                        in
                            if not (isSameType(tenv, (#ty body'), result_ty, pos))
                            then Err.error pos ("Function body type doesn't match return type in function " ^ S.name name)
                            else ()
                        end 
                    fun foldfundec (fundec, ()) = checkfundec fundec
                    fun checkDuplicates({name, params, body, pos, result}, seenList) = 
                        if List.exists (fn y => String.compare(S.name name, y) = EQUAL) seenList
                        then (Err.error pos "error : two types of same name in mutually recursive fundec"; seenList)
                        else (S.name name)::seenList
                in
                    foldl checkDuplicates [] fundeclist;
                    foldr foldfundec () fundeclist;
                    {venv=venv', tenv=tenv}
                end
                *)
            and folddec(dec, {venv, tenv}) = trdec(venv, tenv, dec)
        in
            foldl folddec {venv=venv, tenv=tenv} decs
        end
        
and transTy(tenv, ty) =
  let 
    fun trty(tenv, A.NameTy (name, _)) = 
      (case S.look(tenv, name) of
        SOME _ => T.NAME(name, ref(NONE))
      | NONE => (Err.error 0 ("error: unrecognized name type: " ^ S.name name); T.NAME(name, ref(NONE)))
      )    
    | trty(tenv, A.RecordTy(fields)) = 
        let 
          fun trfields {name, escape, typ, pos} =
            case S.look(tenv, typ) of
              SOME(ty) => (name, ty)
            | NONE => (Err.error pos ("error: undefined type in record field: " ^ S.name typ); (name, T.NIL))  
            (* TODO: fix one of these things idk what's wrong @MICHELLE *)
            fun recGen() = foldl (fn (a, b) => trfields(a)::b) [] fields
        in 
            recGen();
            T.RECORD (recGen, ref ())
        end
    | trty(tenv, A.ArrayTy(sym, pos)) = 
        T.ARRAY (transTy (tenv, A.NameTy (sym, pos)), ref ()) (* FIX: may need to call less scoped function *)
  in
    trty(tenv, ty)
  end

(* transProg needs to take in expression to translate, run transExp, and return unit *)
fun transProg(exp_to_translate : A.exp) = 
    (transExp (E.base_venv, E.base_tenv, exp_to_translate); ())

end 
