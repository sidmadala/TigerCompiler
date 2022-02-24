structure A = Absyn
structure E = Env
structure S = Symbol
structure Err = ErrorMsg
structure T = Types

structure Semant =
struct

type venv = E.enventry S.table
type tenv = E.ty S.table

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
  | checkTyEq(_) = Error.error pos "error: types not equal"

fun isSameType(tenv, T.UNIT, T.UNIT, pos) = true
  | isSameType(tenv, T.INT, T.INT, pos) = true
  | isSameType(tenv, T.STRING, T.STRING, pos) = true
  | isSameType(tenv, T.RECORD(_, u1), T.RECORD(_, u2), pos) = u1 = u2
  | isSameType(tenv, T.RECORD(_), T.UNIT, pos) = true
  | isSameType(tenv, T.UNIT, T.RECORD(_), pos) = true
  | isSameType(tenv T.ARRAY(_, u1), T.ARRAY(_, u2), pos) = u1 = u2
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
        | trexp(A.SeqExp((exp, pos)::l)) = (trexp(exp); trexp(l))
        (* OpExp: check for type equality*)
        | trexp(A.OpExp{left, operation, right, pos}) = 
          (case operation of 
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
              val {venv', tenv'} = transDecs(venv, tenv, decs)
            in
              transExp(venv', tenv', body)
            end
        (* BreakExp *)
        | trexp(A.BreakExp(pos)) = (if !loopLevel = 0 then Err.error pos "illegal break" else (); {exp = (), ty = T.UNIT}) 
        (* WhileExp *)
        | trexp(A.WhileExp{test, body, pos}) = (checkInt(trexp test, pos);
        incLoopLevel(); if not isSameType(tenv, #ty (trexp body), T.UNIT, pos) then Err.error "while loop should return UNIT" else (); decLoopLevel(); {exp = (), ty = T.UNIT})
        (* ForExp *)
        | trexp(A.ForExp{var, escape, lo, hi, body, pos}) = (checkInt(trexp lo, pos); checkInt(trexp hi, pos); incLoopLevel(); if not isSameType(tenv,
        #ty (transExp(S.enter(venv, var, E.VarEntry{ty = T.INT}), tenv, body)), T.UNIT, pos) then Err.error "for loop should return UNIT"; decLoopLevel(); {exp = (), ty = T.UNIT}) 
        (* IfExp *)
        | trexp(A.IfExp{test, then', else', pos}) = (checkInt(trexp test, pos);
        if not isSome(else') 
        then 
            (if not isSameType(tenv, #ty (trexp then'), T.UNIT, pos) 
             then Err.error pos "then should return UNIT" else ();
             {exp = (), ty = T.UNIT})
        else 
            (if not isSameType(tenv, #ty (trexp then'), #ty (trexp valOf(else')), pos) 
             then Err.error pos "then and else should return the same type" else ();
             {exp = (), ty = #ty (trexp then')}))
        (* AssignExp *)
        | trexp(A.AssignExp{var, exp, pos}) = 
          (if not isSameType(tenv, #ty (trvar(var)), #ty (trexp(exp)), pos)
              then Err.error pos "error: var and exp types don't match" else ();
              {exp = () , ty = T.UNIT} 
              )
        (* CallExp *)
        | trexp(A.CallExp{func, args, pos}) = 
        (* 1. S.look if function exists
        2. check if argument typing works out  *)
          (let 
          fun checkFunParams(f::formals, a::args, pos) = 
                if isSameType(tenv, f, a, pos) 
                then checkFunParams(formals, args, pos) 
                else Err.error pos "error: argument mismatch, expected" ^ (f) ^ "got" ^ (a) 
              | checkFunParams([], a::args, pos) = Err.error pos "error: too many arguments given"
              | checkFunParams(f::formals, [], pos) = Err.error pos "error: not enough arguments given"
              | checkFunParams([], [], pos) = ()
          in 
            case S.look(venv, func) of
              SOME(Env.FunEntry{formals, resultTy}) => (checkFunParams(formals, args, pos); {exp=(), ty = resultTy})
              | SOME(_) => (Err.error pos "error: why this is not a function (does this happen? idk)"; {exp=(), ty=T.UNIT})
              | NONE => (Err.error pos "error: function not declared"; {exp=(), ty=T.UNIT})
          end
          )
      and trvar(A.SimpleVar(sym, pos)) =
        (case S.look(venv, sym) of
              SOME(Env.VarEntry({ty})) => {exp=(), ty=ty} 
            | SOME(Env.FunEntry({formals, result})) => {exp=(), ty=result} 
            | NONE => (Err.error pos ("error: variable not declared" ^ S.name sym); {exp=(), ty=T.BOTTOM})
        )
        | trvar(A.FieldVar(var, sym, pos)) = 
            (case trvar var of 
                {exp=(), ty=T.RECORD(fieldTys, unique)} =>
                    let
                      val fields = fieldTys()
                      fun getFieldType ((fieldSym, fieldTy)::l, id, pos) =
                        if String.compare(S.name fieldSym, S.name sym) = EQUAL 
                        then 
                          case S.look(tenv, fieldTy) of
                            SOME(ty) => ty
                            |NONE => (Err.error pos ("error: type does not exist in fields"); T.BOTTOM)
                        else getFieldType(l, id, pos)
                        | getFieldType ([], id, pos) = (Err.error pos ("error: field does not exist in record"); T.BOTTOM)
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

 (* and transDecs(venv, tenv, []) = {venv = venv, tenv = tenv}
  | transDecs(venv, tenv, decs) = 
    (* TODO *)
    and transTy(tenv, ty)=  *)

(* transProg needs to take in expression to translate, run transExp, and return unit *)
fun transProg(exp_to_translate : A.exp) = 
    (transExp (E.base_venv, E.base_tenv, exp_to_translate); ())

end 
