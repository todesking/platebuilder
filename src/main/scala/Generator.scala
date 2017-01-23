package com.todesking.platebuilder

abstract class Generator[T <: Type] {
  def dependencies: Set[VarID]
}
object Generator {
  case class Given[T <: Type](descripiton: Option[String]) extends Generator[T] {
    override def dependencies = Set()
  }
  case class Stochastic[T <: Type](expr: Option[Expr], override val dependencies: Set[VarID]) extends Generator[T] {
  }
  case class Deterministic[T <: Type](expr: Option[Expr], override val dependencies: Set[VarID]) extends Generator[T] {
  }

  case class Expr(parts: Seq[String], args: Seq[Any]) {
    require(args.size == parts.size - 1)
  }
}

