package com.todesking.platebuilder
case class VarID(str: String) {
  def asIndex: IndexID = IndexID(str)
}
