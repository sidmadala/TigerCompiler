structure FindEscape: FINDESCAPE = 
struct
    structure A = Absyn
    structure S = Symbol
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table
    
    fun traverseVar(env:escEnv, d:depth, s:Absyn.var): unit =
        let
            fun trvar(A.SimpleVar(sym, pos)) = 
                (* Bulk of work here to check if variable escapes *)
                (case S.look(env, sym) of 
                    SOME(d', escape) => if d > d' then escape := true else ()
                  | NONE => () 
                )
              | trvar(A.SubscriptVar(var, exp, pos)) = (trvar var; traverseExp(env, d, exp))
              | trvar(A.FieldVar(var, sym, pos)) = trvar(var)
        in
            trvar s
        end
    and traverseExp(env:escEnv, d:depth, s:Absyn.exp): unit =
        let 
            fun trexp(A.NilExp) = ()
              | trexp(A.IntExp(num)) = ()
              | trexp(A.StringExp(str)) = ()
              | trexp(A.BreakExp(pos)) = ()
              | trexp(A.VarExp(var)) = traverseVar(env, d, var)
              | trexp(A.OpExp {left, oper, right, pos}) = (trexp left; trexp right)
              | trexp(A.CallExp {func, args, pos}) = List.app trexp args 
              | trexp(A.RecordExp {fields, typ, pos}) = List.app (fn (sym, exp, pos) => trexp exp) fields
              | trexp(A.ArrayExp {typ, size, init, pos}) = (trexp size; trexp init)
              | trexp(A.IfExp {test, then', else', pos}) = (trexp test; trexp then'; case else' of SOME(exp') => trexp exp' | NONE => ())
              | trexp(A.ForExp {var, escape, lo, hi, body, pos}) = (trexp lo; trexp hi; traverseExp(S.enter(env, var, (d, escape)), d, body))
              | trexp(A.WhileExp {test, body, pos}) = (trexp test; trexp body) 
              | trexp(A.LetExp {decs, body, pos}) = traverseExp(traverseDecs(env, d, decs), d, body)
              | trexp(A.AssignExp {var, exp, pos}) = (traverseVar(env, d, var); trexp exp) 
              | trexp(A.SeqExp expList) = List.app (fn (exp, pos) => trexp exp) expList
        in
            trexp s
        end
    and traverseDecs(env:escEnv, d:depth, s: Absyn.dec list): escEnv = 
        let 
            fun trdec(A.VarDec {name, escape, typ, init, pos}, env) = 
                let 
                    val env' =  S.enter(env, name, (d, escape))
                in
                    traverseExp(env', d, init); 
                    env'
                end
              | trdec(A.TypeDec(tydeclist), env) = env
              | trdec(A.FunctionDec(fundeclist), env) = 
                let
                    fun addParamEnv({name, escape, typ, pos}, env) = S.enter(env, name, (d+1, escape))
                    (*BUG: where should this 'escape' come from?*)
                    fun trfundec({name, params, result, body, pos}, env) = traverseExp((foldl addParamEnv env params), d + 1, body)
                in
                    (*BUG: something wrong with fundeclist as the param of trfundec idk*)
                    List.app trfundec fundeclist; 
                    env
                end
        in
            foldl trdec env s
        end 

    (* Main function for structure *)
    fun findEscape(prog: Absyn.exp) : unit = traverseExp(S.empty, 0, prog)

end
