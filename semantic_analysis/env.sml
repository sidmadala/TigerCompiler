structure Env :> ENV = 
struct
  type access = unit
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  val base_tenv = foldl (fn((s, t), ans) => Symbol.enter(ans, s, t)) Symbol.empty [(Symbol.symbol "int", Types.INT), (Symbol.symbol "string", Types.STRING)]
  
  val base_venv = foldl (fn((s, t), ans) => Symbol.enter(ans, s, t)) Symbol.empty 
  [
    (Symbol.symbol "print", FunEntry {formals = [Types.STRING], result = Types.UNIT}), 
    (Symbol.symbol "flush", FunEntry {formals = [], result = Types.UNIT}), 
    (Symbol.symbol "getchar", FunEntry {formals = [], result = Types.STRING}), 
    (Symbol.symbol "ord", FunEntry {formals = [Types.STRING], result = Types.INT}), 
    (Symbol.symbol "chr", FunEntry {formals = [Types.INT], result = Types.STRING}), 
    (Symbol.symbol "size", FunEntry {formals = [Types.STRING], result = Types.INT}), 
    (Symbol.symbol "substring", FunEntry {formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING}), 
    (Symbol.symbol "concat", FunEntry {formals = [Types.STRING, Types.STRING], result = Types.STRING}), 
    (Symbol.symbol "not", FunEntry {formals = [Types.INT], result = Types.INT}), 
    (Symbol.symbol "exit", FunEntry {formals = [Types.INT], result = Types.INT}) 
  ]
end

