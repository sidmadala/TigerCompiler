signature ENV = 
sig 
    type access
    type ty = Types.ty
    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty}

    (* Predefined types and functions *)
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table 
end
