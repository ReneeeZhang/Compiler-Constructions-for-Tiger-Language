signature ENV = 
sig
  type access
  type ty
  type enventry = {access: access, ty: ty} 
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end
