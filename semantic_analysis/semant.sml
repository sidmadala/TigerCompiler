structure A = Absyn

structure Semant =
struct

type venv = Env.enventry Symbol.table
type tenv = ty Symbol.table

type expty = {exp: Translate.exp, ty: Types.ty}

fun transExp() = 
fun transVar() = 
fun transDec() =
fun transTy() = 

fun transProg(exp_to_translate : A.exp) = 
    (transExp (Env.base_venv, Env.base_tenv, exp_to_translate); ())

end