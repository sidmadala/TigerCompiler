structure A = Absyn
structure E = Env
structure S = Symbol

structure Semant =
struct

type venv = E.enventry S.table
type tenv = E.ty S.table

type expty = {exp: Translate.exp, ty: Types.ty}

fun transExp(venv, tenv, exp) =
    let
      fun trexp(A.NilExp) = {exp = (), ty = Types.NIL}
        | trexp(A.IntExp(i)) = {exp = (), ty = Types.INT}
        | trexp(A.StringExp(s, pos)) = {exp = (), ty = Types.STRING}
        (* SeqExp *)
        | trexp(A.SeqExp([])) = {exp = (), ty = Types.UNIT}
        | trexp(A.SeqExp([(exp, pos)])) = trexp(exp)
        | trexp(A.SeqExp((exp, pos)::l)) = (trexp(exp); trexp(l))

      and trevar() =
    in
      trexp(exp)
    end


  
  
  
fun transVar() = 
fun transDec() =
fun transTy() = 

fun transProg(exp_to_translate : A.exp) = 
    (transExp (E.base_venv, E.base_tenv, exp_to_translate); ())

end
