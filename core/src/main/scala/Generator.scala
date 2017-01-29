package com.todesking.platebuilder

sealed abstract class Generator[T <: Type] {
  def dependencies: Set[VarID]
}
object Generator {
  case class Expr[T <: Type](stochastic: Boolean, override val dependencies: Set[VarID], parts: Seq[String], args: Seq[Any]) extends Generator[T] {
    require(args.size == parts.size - 1)
  }
  case class Const[T <: Type](repr: String) extends Generator[T] {
    override def dependencies = Set()
  }
  case class Given[T <: Type]() extends Generator[T] {
    override def dependencies = Set()
  }
}

