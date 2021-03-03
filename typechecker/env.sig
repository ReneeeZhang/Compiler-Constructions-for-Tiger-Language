signature ENV = 
sig
  type ty = Types.ty
  type enventry = {access: unit, ty: ty} 
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end
