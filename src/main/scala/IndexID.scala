package com.todesking.platebuilder

// TODO: IndexType(Var|Constant)
case class IndexID(str: String) {
  def asVar: VarID = VarID(str)
}
