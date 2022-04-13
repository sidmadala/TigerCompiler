structure Env :> ENV = 
struct
  structure Tr = Translate
  structure T = Types 

  type access = unit
  type ty = Types.ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty}

  val base_tenv = foldl (fn((s, t), ans) => Symbol.enter(ans, s, t)) Symbol.empty [(Symbol.symbol "int", Types.INT), (Symbol.symbol "string", Types.STRING)]
  
  val base_venv = foldl (fn((s, t), ans) => Symbol.enter(ans, s, t)) Symbol.empty 
  [
    (Symbol.symbol "print", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [Types.STRING], result = Types.UNIT}), 
    (Symbol.symbol "flush", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [], result = Types.UNIT}), 
    (Symbol.symbol "getchar", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [], result = Types.STRING}), 
    (Symbol.symbol "ord", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [Types.STRING], result = Types.INT}), 
    (Symbol.symbol "chr", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [Types.INT], result = Types.STRING}), 
    (Symbol.symbol "size", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [Types.STRING], result = Types.INT}), 
    (Symbol.symbol "substring", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING}), 
    (Symbol.symbol "concat", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [Types.STRING, Types.STRING], result = Types.STRING}), 
    (Symbol.symbol "not", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [Types.INT], result = Types.INT}), 
    (Symbol.symbol "exit", FunEntry {level = Tr.outermost, label = Temp.newlabel(), formals = [Types.INT], result = Types.INT}) 
  ]
end

