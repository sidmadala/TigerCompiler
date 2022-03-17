structure Err = ErrorMsg
structure A = Absyn
structure E = Env
structure S = Symbol
structure Tr = Translate
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
  | isSubType(T.UNIT, T.UNIT) = true
  | isSubType(T.NIL, T.RECORD(_)) = true
  | isSubType(T.INT, T.INT) = true
  | isSubType(T.STRING, T.STRING) = true
  | isSubType(T.RECORD(_, u1), T.RECORD(_, u2)) = u1 = u2
  | isSubType(T.ARRAY(_, u1), T.ARRAY(_, u2)) = u1 = u2
  | isSubType(T.NAME(s1, _), T.NAME(s2, _)) = String.compare(S.name s1, S.name s2) = EQUAL 
  | isSubType(_, _) = false

fun tyToString(T.NIL) = "NIL"
  | tyToString(T.NAME(_)) = "NAME"
  | tyToString(T.RECORD(_)) = "RECORD"
  | tyToString(T.ARRAY(_)) = "ARRAY"
  | tyToString(T.UNIT) = "UNIT"
  | tyToString(T.BOTTOM) = "BOTTOM"
  | tyToString(T.INT) = "INT"
  | tyToString(T.STRING) = "STRING"

fun checkTyOrder(T.NIL, T.NIL, "eq", pos, errorMsg) = Err.error pos "nil cannot compare with nil"
  | checkTyOrder(T.RECORD(_), T.NIL, "eq", pos, errorMsg) = () 
  | checkTyOrder(t1, t2, "sub", pos, errorMsg) = if isSubType(t1, t2) then () else Err.error pos errorMsg
  | checkTyOrder(t1, t2, "super", pos, errorMsg) = checkTyOrder(t2, t1, "sub", pos ,errorMsg)
  | checkTyOrder(t1, t2, "eq", pos, errorMsg) = if isSubType(t1, t2) andalso isSubType(t2, t1) then () else Err.error pos errorMsg
  | checkTyOrder(t1, t2, "assign", pos, errorMsg) = checkTyOrder(t1, t2, "super", pos, errorMsg)
  | checkTyOrder(T.INT, T.INT, "comp", pos, errorMsg) = ()
  | checkTyOrder(T.STRING, T.STRING, "comp", pos, errorMsg) = ()
  | checkTyOrder(T.INT, T.INT, "arith", pos, errorMsg) = ()
  | checkTyOrder(t1, t2, "neq", pos, errorMsg) = if isSubType(t1, t2) = false
  orelse isSubType(t2, t1) = false then () else Err.error pos errorMsg
  | checkTyOrder(_, _, _, pos, errorMsg) = Err.error pos errorMsg


(* helper functions *)
fun getType(SOME(ty)) = ty
    | getType(NONE) = T.BOTTOM

fun actualTy(tenv, ty) = 
    case ty of
        T.NAME(name, tyref) => actualTy(tenv, getType(S.look(tenv, name)))
        | someTy => someTy

(* beginning of main transExp function *)
fun transExp(venv, tenv, exp, level, break) =
    let
      fun trexp(A.NilExp) = {exp = (), ty = T.NIL}
        | trexp(A.IntExp(i)) = {exp = (), ty = T.INT}
        | trexp(A.StringExp(s, pos)) = {exp = (), ty = T.STRING}
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
          if isInLoop() then () else Err.error pos "illegal break"; 
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
          checkTyOrder(actualTy(tenv, #ty (trvar(var))), #ty (trexp(exp)), "assign", pos, "error: var and exp types don't match");
          {exp = () , ty = T.UNIT} 
          )

        (* CallExp 🐶*)
        | trexp(A.CallExp{func, args, pos}) = 
        (* 1. S.look if function exists
        2. check if argument typing works out  *)
          (let 
          (* TODO: "eq" or "super" *)
          fun checkFunParams(f::formals, a::args, pos) = (checkTyOrder(f, #ty
            (trexp a), "super", pos, "error: argument mismatch"); checkFunParams(formals, args, pos))
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
              SOME(x) =>
                (case actualTy(tenv, x) of
                  T.ARRAY(ty, unique) => (
                    checkTyOrder(#ty (trexp size), T.INT, "eq", pos, "not an integer");
                    checkTyOrder(actualTy(tenv, ty), #ty (trexp init), "super", pos, "error: array type and initializing exp differ");
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
                      (case actualTy(tenv,x) of
                          T.RECORD(f, _) => 
                                let 
                                    fun check(((symbol, exp, pos), (name, ty)), ()) = 
                                        if String.compare(S.name symbol, S.name name) <> EQUAL
                                        then Err.error pos ("field name doesn't match Given:" ^ S.name symbol ^ "Requred:" ^ S.name name)
                                        else checkTyOrder(actualTy(tenv, ty),
                                        actualTy(tenv, #ty (trexp exp)),
                                        "super", pos, 
                                        "field type doesn't match Required:" ^ (tyToString(actualTy(tenv, ty)) ^ "Given:" ^
                                        (tyToString(actualTy(tenv, #ty (trexp
                                        exp))))))
                                in
                                    if List.length(f(tenv)) <> List.length(fields)
                                    then (Err.error pos ("record list is wrong length: " ^ S.name typ); {exp=(), ty= T.NIL})
                                    else (foldl check () (ListPair.zip(fields,
                                    f(tenv))); {exp=(), ty= actualTy(tenv, x)})
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
            | NONE => (Err.error pos ("error: variable not declared " ^ S.name sym); {exp=(), ty= T.BOTTOM})
        )
          (* FieldVar *)
        | trvar(A.FieldVar(var, sym, pos)) = 
            (case actualTy(tenv, #ty(trvar var)) of 
                T.RECORD(fieldTys, unique) =>
                    let
                      val fields = fieldTys(tenv)
                      fun getFieldType((fieldSym, fieldTy)::l, id : Absyn.symbol, pos : Absyn.pos) =
                            if String.compare(S.name fieldSym, S.name sym) = EQUAL 
                            then actualTy(tenv, fieldTy) 
                            else getFieldType(l, id, pos)
                        | getFieldType([], id, pos) = (Err.error pos ("error: field does not exist in record"); T.BOTTOM)
                    in
                      {exp=(), ty=getFieldType(fields, sym, pos)}
                    end
                | _ => (Err.error pos ("error: not a record"); {exp=(), ty=T.BOTTOM})
            )
            (* SubscriptVar *)
        | trvar(A.SubscriptVar(var, exp, pos)) =
              (case actualTy(tenv, #ty (trvar var)) of
                T.ARRAY(arrTy, unique) => (checkTyOrder(#ty (trexp
                exp), T.INT, "eq", pos, "not an integer"); {exp=(), ty=actualTy(tenv, arrTy)})
                | _ => (Err.error pos ("error: not an array"); {exp=(), ty=T.BOTTOM})  
              )    
    in
      trexp(exp)
    end

and transDec(venv, tenv, decs, level, break) = 
        let 
          (* VarDec 🐶 *)
          fun trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos}), expList) =
              
                let
                val initTy = #ty (transExp(venv, tenv, init, level, break));
                val access' = Tr.allocLocal level (!escape)
                fun appendExpList() =
                      let
                        val left = Tr.simpleVarIR(access', level)
                        val right = #exp (transExp(venv, tenv, init, level, break))
                      in
                        expList @ [Tr.assignIR(left, right)]
                      end
                in
                case typ of
                    SOME(symbol, pos) =>
                        (case S.look(tenv, symbol) of
                            SOME ty => (checkTyOrder(actualTy(tenv, ty), initTy, "super", pos, "types mismatch");
                                       {venv=S.enter(venv, name, (Env.VarEntry{access = access', ty = actualTy(tenv, ty)})), tenv = tenv, expList = appendExpList()})
                          | NONE => (Err.error pos "type not recognized"; {venv = venv, tenv = tenv, expList = appendExpList()})
                        )
                  | NONE =>
                        (if String.compare(tyToString(initTy), "NIL") = EQUAL then Err.error pos "error: initializing nil expressions not constrained by record type" else ();
                        {venv=S.enter(venv, name, (Env.VarEntry{access = access', ty = initTy})),
                        tenv = tenv, expList = appendExpList()})
                        
                end
                
          (* TypeDec  TODO: should we break the cycle here or add extra stuff in actualTy to avoid inifite loop?*)
          | trdec(venv, tenv, A.TypeDec(tydeclist), expList) =
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
                {tenv = tenvActualTy, venv = venv, expList = expList}
              end
             (* FunctionDec  TODO: 1. use actualTy?? 2. should return type use
             * sub or eq *)
          | trdec(venv, tenv, A.FunctionDec(fundeclist), expList) =
                let 
                    fun transrt (rt, pos) =
                        (case S.look(tenv, rt) of 
                            SOME(ty) => actualTy(tenv, ty)
                          | NONE => (Err.error pos ("Return type unrecognized: " ^ S.name rt); T.BOTTOM)
                        )
                    fun transparam {name, escape, typ, pos} = 
                        (case S.look(tenv, typ) of
                            SOME ty => {name = name, ty = actualTy(tenv, ty)}
                          | NONE => (Err.error pos ("Parameter type unrecognized:" ^ S.name typ); {name = name, ty = T.BOTTOM})
                        )
                    fun enterFuncs (func, venv) = 
                        let
                          val newlabel = Temp.newlabel()
                          fun getEscape {name=name', escape=escape', typ=typ', pos=pos'} = !escape'
                          fun genEscapeList params' = map getEscape params'
                        in
                          case func of 
                              {name, params, body, pos, result=SOME(rt, pos')} =>
                                    S.enter(venv, name, Env.FunEntry{level=Translate.newLevel {parent=level, name=newlabel, formals=genEscapeList params},
                                                                     label=newlabel, formals= map #ty (map transparam params), result=transrt rt})
                            | {name, params, body, pos, result=NONE} =>
                                    S.enter(venv, name, Env.FunEntry{level=Translate.newLevel {parent=level, name=newlabel, formals=genEscapeList params},
                                                                     label=newlabel, formals= map #ty (map transparam params), result=T.UNIT})
                        end
                    (*fun enterFuncs ({name, params, body, pos, result=SOME(rt, pos')}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty
                            (map transparam params), result=transrt(rt,pos)})
                      | enterFuncs ({name, params, body, pos, result=NONE}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=T.UNIT})*)
                    val venv' = foldl enterFuncs venv fundeclist
                    fun checkfundec({name, params, body, pos, result}) = 
                        let 
                            val newLevel = 
                                (case S.look(venv', name) of
                                     SOME(Env.FunEntry({level=level', label=_, formals=_, result=_})) => level'
                                   | _ => Tr.newLevel {parent=Tr.outermost, name=Temp.newlabel(), formals=[]}
                                   )
                            val result_ty = 
                                (case result of
                                    SOME(rt, pos') => transrt rt
                                  | NONE => T.UNIT
                                )
                            val params' = map transparam params
                            val allocatedFormals = Tr.formals newLevel
                            fun enterparam ({name, escape, ty, pos}, (venv, curIndex)) = 
                              (S.enter(venv, name, Env.VarEntry{access=List.nth(allocatedFormals, curIndex),
                                                               ty=ty}), curIndex + 1)
                            val venv'' = #1 (foldl enterparam (venv', 1) params')
                            val body' = transExp (venv'', tenv, body, newLevel, break)
                        in
                            Tr.procEntryExit {level=newLevel, body=(#exp body')};
                            checkTyOrder(#ty bodyResult, resultTy, "sub", pos, "Function body type doesn't match return type in function " ^ S.name name)
                        end 
                    (*fun checkfundec({name, params, body, pos, result}) = 
                        let 
                            val resultTy = 
                                (case result of
                                    SOME(rt, pos') => transrt(rt,pos')
                                  | NONE => T.UNIT
                                )
                            val venv'' = foldl (fn({name, ty}, ans) => S.enter(ans, name, Env.VarEntry{ty=ty})) venv' (map transparam params)
                            val bodyResult = transExp (venv'', tenv, body)
                        in
                          checkTyOrder(#ty bodyResult, resultTy, "sub", pos, "Function body type doesn't match return type in function " ^ S.name name)
                        end *)
                    fun foldfundec (fundec, ()) = checkfundec fundec
                in
                    foldl (fn({name, params, body, pos, result}, l) => if List.exists (fn s => String.compare(s, S.name name) = EQUAL) l then (Err.error pos "error : two types of same name in mutually recursive fundec"; l) else (S.name name :: l))  [] fundeclist;
                    foldr foldfundec () fundeclist;
                    {venv=venv', tenv=tenv, expList = expList}
                end
            and folddec(dec, {venv, tenv, expList}) = trdec(venv, tenv, dec, expList)
        in
            foldl folddec {venv=venv, tenv=tenv, expList=[]} decs
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
          fun trfields (tenv, {name, escape, typ, pos}) =
            case S.look(tenv, typ) of
              SOME(ty) => (name, ty)
            | NONE => (Err.error pos ("error: undefined type in record field: " ^ S.name typ); (name, T.NIL))  
            (* TODO: fix one of these things idk what's wrong @MICHELLE *)
            fun recGen(tenv) = foldl (fn (a, b) => b @ [trfields(tenv, a)]) [] fields
        in
            recGen(tenv); (* check TYPE!!!! MUST KEEP!! *)
            T.RECORD (recGen, ref ())
        end
      (* ArrayTy 🐶*)
    | trty(tenv, A.ArrayTy(sym, pos)) = 
        T.ARRAY((transTy(tenv, A.NameTy(sym, pos))), ref ())
  in
    trty(tenv, ty)
  end

(* transProg needs to take in expression to translate, run transExp, and return unit *)
fun transProg(exp_to_translate : A.exp) = 
    (transExp (E.base_venv, E.base_tenv, exp_to_translate); ())

end 
