package com.todesking.platebuilder

abstract class Generator[T <: Type] {
  def dependencies: Set[VarID]
  def description: Option[String]
}
object Generator {
  case class Given[T <: Type](override val description: Option[String]) extends Generator[T] {
    override def dependencies = Set()
  }
  case class Sampled[T <: Type](override val description: Option[String], override val dependencies: Set[VarID]) extends Generator[T]
  case class Computed[T <: Type](override val description: Option[String], override val dependencies: Set[VarID]) extends Generator[T]
}

