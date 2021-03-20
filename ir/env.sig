signature ENV = 
sig
  type ty = Types.ty
  type enventry = {access: unit, ty: ty} (* Modify to Translate.access *)
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end
