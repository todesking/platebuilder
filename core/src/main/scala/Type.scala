package com.todesking.platebuilder

sealed abstract class Type {
  def bareType: Type.Scalar
  def dimension: Seq[IndexID]
}
object Type {
  sealed abstract class Scalar extends Type {
    override def bareType = this
    override def dimension = Seq()
  }
  type Real = Real.type
  case object Real extends Scalar
  type Binary = Binary.type
  case object Binary extends Scalar
  case class Size[A <: String](indexID: IndexID) extends Scalar
  case class Category[A <: String](size: Size[A]) extends Scalar

  case class Vec[I <: String, A <: Type](indexID: IndexID, elm: A) extends Type {
    def elementType: A = elm
    override def bareType = elm.bareType
    override def dimension = Seq(indexID) ++ elm.dimension
  }
}

