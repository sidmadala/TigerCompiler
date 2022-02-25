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

fun printType ty =
  case ty of
        T.RECORD(_, _) => print "type is record\n"
      | T.NIL => print "type is nil\n"
      | T.INT => print "type is int\n"
      | T.STRING => print "type is string\n"
      | T.ARRAY(arrTy, _) => (print "array of: "; printType arrTy)
      | T.NAME(sym, _) => print ("name type is " ^ Symbol.name sym ^ "\n")
      | T.UNIT => print "type is unit\n"
      | T.BOTTOM => print "type is bottom\n"

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
  | typeExtractor(tenv, typ, pos) = typ

fun getTyFromSymbol(tenv, sym, pos) = 
      case S.look(tenv, sym) of 
        SOME(typ) => (typeExtractor(tenv, typ, pos))
        | NONE => (ErrorMsg.error pos ("type not yet defined"); T.BOTTOM)

fun checkInt({exp=_, ty=T.INT}, pos) = ()
    | checkInt ({exp=_, ty=_ }, pos) = Err.error pos "error: not an integer"

fun checkTyComp({exp=_, ty = T.INT}, {exp=_, ty = T.INT}, pos) = ()
  | checkTyComp({exp=_, ty = T.STRING}, {exp=_, ty = T.STRING}, pos) = ()
  | checkTyComp(_, _, pos) = Err.error pos "error: not comparable"

fun checkTyCompatible(T.INT, T.INT, pos) = ()
  | checkTyCompatible(T.STRING, T.STRING, pos) = ()
  | checkTyCompatible(T.UNIT, T.UNIT, pos) = ()
  | checkTyCompatible(T.RECORD(_), T.NIL, pos) = ()
  | checkTyCompatible(_, T.BOTTOM, pos) = ()
  | checkTyCompatible(T.UNIT,_, pos) = ()
  | checkTyCompatible(T.RECORD(_, u1), T.RECORD(_, u2), pos) =
    if u1 = u2 then () else Err.error pos "error: record types mismatch"
  | checkTyCompatible(T.ARRAY(_, u1), T.ARRAY(_, u2), pos) =
    if u1 = u2 then () else Err.error pos "error: array types mismatch"
  | checkTyCompatible(_, _, pos) = Err.error pos "error: types not compatible"

fun isSameType(tenv, T.UNIT, T.UNIT, pos : Absyn.pos) = true
  | isSameType(tenv, T.INT, T.INT, pos) = true
  | isSameType(tenv, T.STRING, T.STRING, pos) = true
  | isSameType(tenv, T.RECORD(_, u1), T.RECORD(_, u2), pos) = (u1 = u2)
  | isSameType(tenv, T.RECORD(_), T.UNIT, pos) = true
  | isSameType(tenv, T.UNIT, T.RECORD(_), pos) = true
  | isSameType(tenv, T.ARRAY(_, u1), T.ARRAY(_, u2), pos) = (u1 = u2)
  | isSameType(tenv, T.NAME(s1, _), T.NAME(s2, _), pos) = String.compare(S.name s1, S.name s2) = EQUAL
  | isSameType(tenv, ty1, ty2, pos) = false

(* Checks for duplicate type declaration in tenv *)
fun checkTyDecDuplicates({name, ty, pos}, seen) = 
  if List.exists (fn elem => String.compare(S.name name, elem) = EQUAL) seen
  then (Err.error pos "error : duplicate types"; seen)
  else (S.name name)::seen

(* Checks for duplicate type declaration in fundeclist *)
fun checkFunDecDuplicates({name, params, body, pos, result}, observed) = 
  if List.exists (fn elem => String.compare(S.name name, elem) = EQUAL) observed
  then (Err.error pos "error : two types of same name in mutually recursive fundec"; observed)
  else (S.name name)::observed

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
            | A.EqOp => (checkTyCompatible(#ty(trexp left), #ty(trexp right), pos); {exp=(), ty=T.INT})
            | A.NeqOp => (checkTyCompatible(#ty(trexp left), #ty(trexp right), pos); {exp=(), ty=T.INT})
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
            fun checkFunParams(f::formals, a::args, pos) = 
                  if isSameType(tenv, f, #ty (trexp a), pos) 
                  then checkFunParams(formals, args, pos) 
                  else (Err.error pos "error: argument mismatch"; ())
                | checkFunParams([], a::args, pos) = (Err.error pos "error: too many arguments given"; ())
                | checkFunParams(f::formals, [], pos) = (Err.error pos "error: not enough arguments given"; ())
                | checkFunParams([], [], pos) = ()
          in 
            case S.look(venv, func) of
              SOME(Env.FunEntry({formals, result})) => (checkFunParams(formals, args, pos); {exp=(), ty=result})
              | SOME(_) => (Err.error pos "error: not a function, but a var?"; {exp=(), ty=T.BOTTOM})
              | NONE => (Err.error pos "error: function not declared"; {exp=(), ty=T.BOTTOM})
          end
          )
        | trexp(A.ArrayExp({typ, size, init, pos})) =
            (case getTyFromSymbol(tenv, typ, pos) of 
              (T.ARRAY(ty, unique)) => 
                (checkInt(trexp size, pos);
                if isSameType(tenv, #ty (trexp(init)), actualTy(tenv, ty), pos)
                then ()
                else Err.error pos "error: array type and initializing exp differ";
                {exp=(), ty=actualTy(tenv, getTyFromSymbol(tenv, typ, pos))}
                )
              |_ => (Err.error pos "error: not an array"; {exp=(), ty=T.BOTTOM}))
        | trexp (A.RecordExp({fields, typ, pos})) = 
          (case S.look(tenv, typ) of
            SOME ty => 
              (case ty of
                T.RECORD(genfields, _) => 
                  let 
                    val recFields = genfields()

                    fun getFieldType (name: string, []) = T.BOTTOM
                      | getFieldType (name: string, (sym, exp, pos)::l) =
                          if String.compare (name, S.name sym) = EQUAL
                          then #ty (trexp exp)
                          else getFieldType(name, l)

                    fun checkFields ((sym, ty), ()) =
                      if (checkTyCompatible(getFieldType(S.name sym, fields), ty, pos) = ())
                      then ()
                      else Err.error pos ("error: actual type doesn't match formal type: " ^ S.name sym)
                  in
                    if (List.length(recFields) = List.length(fields))
                    then (foldr checkFields () recFields; {exp=(), ty=ty})
                    else (Err.error pos ("error: record list is wrong length"); {exp=(), ty=T.BOTTOM})
                  end
              | _ => (Err.error pos ("error: this is not a record"); {exp=(), ty=T.BOTTOM})
              )
          | NONE => (Err.error pos ("error: " ^ S.name typ ^ " does not exist as a record"); {exp=(), ty=T.BOTTOM})
          )
      and trvar(A.SimpleVar(sym, pos)) =
        (case S.look(venv, sym) of
              SOME(Env.VarEntry({ty})) => {exp=(), ty= actualTy(tenv, ty)}
            | SOME(_) => (Err.error pos ("error: variable is a function name?" ^ S.name sym); {exp=(), ty= T.BOTTOM})
            | NONE => (Err.error pos ("error: undefined variable" ^ S.name sym); {exp=(), ty= T.BOTTOM})
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
  let 
    fun trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos})) =
      (case typ of 
        SOME(sym, pos) => 
          (case (S.look(tenv, sym)) of
            SOME(t : T.ty) => 
                    (if checkTyCompatible(actualTy(tenv, t), #ty (transExp(venv, tenv, init)), pos) = ()
                    then ()
                    else Err.error pos "error : mismatched types in vardec";
                    {venv=S.enter(venv, name, (Env.VarEntry{ty=actualTy(tenv, t)})), tenv=tenv})
          | NONE => (Err.error pos ("error: type " ^ S.name sym ^ " not recognized or declared");
                    {venv=venv, tenv=tenv})
          )
      | NONE => 
        let
          val {exp, ty} = transExp(venv, tenv, init)
        in
          {venv = S.enter(venv, name, (Env.VarEntry{ty=ty})), tenv=tenv}
        end
      )
    | trdec(venv, tenv, A.TypeDec(tydeclist)) =
      let
        val temp_tenv = foldl (fn ({name, ty, pos}, tenv') => S.enter(tenv', name, T.BOTTOM)) tenv tydeclist
        fun merge_tenvs({name, ty, pos}, {venv, tenv}) = ({venv=venv, tenv=S.enter(tenv, name, transTy(temp_tenv, ty))})
        val new_tenv = foldl merge_tenvs {venv=venv, tenv=tenv} tydeclist

        (* checking for cycles in declarations *)
        fun checkIllegalCycle({name, ty, pos}, ()) = 
          let
            fun cycleHelper(seen, curr) =
              (case S.look(#tenv new_tenv, curr) of
                    SOME(T.NAME(sym,_)) => if List.exists (fn elem => String.compare(S.name sym, S.name elem) = EQUAL) seen
                                            then Err.error pos "error: cycle exists in type declarations"
                                            else cycleHelper(curr::seen, sym)
                  | _ => () 
              )
          in
            cycleHelper([], name)
          end

      in
        foldl checkTyDecDuplicates [] tydeclist;
        foldl checkIllegalCycle () tydeclist;
        new_tenv
      end

    | trdec(venv, tenv, A.FunctionDec(fundeclist)) =
      let 
        fun transparam {name, escape, typ, pos} = 
          (case S.look(tenv, typ) of
              SOME(t) => {name=name, ty=t}
            | NONE => (Err.error 0 ("Parameter type unrecognized: " ^ S.name typ); {name=name, ty=T.BOTTOM})
          )         (* Like getTyFromSymbol but takes in param fields and returns the param type of the function *)

        (* Place function into venv *)
        fun funvenv ({name, params, body, pos, result=SOME(rt, pos')}, venv) = 
              S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=getTyFromSymbol(tenv,rt,pos')})
          | funvenv ({name, params, body, pos, result=NONE}, venv) =   (* Some functions are void and do not have return values (procedures) *)
              S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=T.UNIT})
       
        (* Temp venv for fundeclist => added to actual venv later so that programs can call these funcs*)
        val venv' = foldr funvenv venv fundeclist

        fun checkfundec({name, params, body, pos, result}) = 
          let 
            val result_ty = 
              (case result of
                  SOME(rt, pos') => getTyFromSymbol(tenv, rt, pos')
                | NONE => T.UNIT
              )
            val params' = map transparam params
            fun enterparam ({name, ty}, venv) = S.enter(venv, name, Env.VarEntry{ty=ty})
            val venv'' = foldl enterparam venv' params'
            val body' = transExp (venv'', tenv, body)
          in
            if (isSameType(tenv, #ty body', result_ty, pos))   
            then ()
            else Err.error pos ("Function body type doesn't match return type in function " ^ S.name name); ()
          end 
          
          (* Add functions to venv *)
          fun addfunvenv (fundec, ()) = checkfundec fundec

      in
        foldl checkFunDecDuplicates [] fundeclist;
        foldr addfunvenv () fundeclist;
        {venv=venv', tenv=tenv}  (* Update venv and return *)
      end 
    and updateenvs(dec, {venv, tenv}) = trdec(venv, tenv, dec)  (* used as function to update environments with declaractions *)
  in 
    foldl updateenvs {venv=venv, tenv=tenv} decs
  end

and transTy(tenv, ty) =
  let 
    fun trty(tenv, A.NameTy(name, _)) = 
      (case S.look(tenv, name) of
        SOME _ => T.NAME(name, ref(NONE))
      | NONE => (Err.error 0 ("error: unrecognized name type: " ^ S.name name); T.NAME(name, ref(NONE)))
      )    
    | trty(tenv, A.RecordTy(fields)) = 
        let 
          fun trfields({name, escape, typ, pos}) =
            case S.look(tenv, typ) of
              SOME(ty) => (name, ty)
            | NONE => (Err.error pos ("error: undefined type in record field: " ^ S.name typ); (name, T.BOTTOM))  
            fun genFields() = foldl (fn (a, b) => trfields(a)::b) [] fields
        in 
            (* QUESTION: why are we running genfields first?? does that make sense? *)
            genFields();
            T.RECORD (genFields, ref ())
        end
    | trty(tenv, A.ArrayTy(sym, pos)) = 
        T.ARRAY((transTy(tenv, A.NameTy(sym, pos))), ref ())
  in
    trty(tenv, ty)
  end

(* transProg needs to take in expression to translate, run transExp, and return unit *)
fun transProg(exp_to_translate : A.exp) = 
    (transExp (E.base_venv, E.base_tenv, exp_to_translate); ())

end 
