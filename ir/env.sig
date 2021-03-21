signature ENV = 
sig
  type ty = Types.ty
  datatype accessty = VarAccess of Translate.access
                    | FuncAccess
  type enventry = {access: accessty, ty: ty} (* Modify to Translate.access *)
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end
