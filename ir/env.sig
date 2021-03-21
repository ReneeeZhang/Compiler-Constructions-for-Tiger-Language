signature ENV = 
sig
  type ty = Types.ty
  datatype accessty = VarAccess of Translate.access (* Can we hide this in sig *)
                    | FuncAccess
  type enventry = {access: accessty, ty: ty}
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end
