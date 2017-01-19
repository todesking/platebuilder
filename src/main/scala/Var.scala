package com.todesking.platebuilder

abstract class Var[T <: Type] {
  import Type.{ Category, Size, Vec }
  def id: VarID
  def varType: T
  def deps: Set[VarID]

  def *[I <: String](dim: Var[Type.Size[I]])(implicit b: Builder): Var[Type.Vec[I, T]] = {
    b.setIndex(id, Seq(dim.id.asIndex))
    new Var.Simple(id, Vec(dim.id.asIndex, varType))
  }

  def *[I1 <: String, I2 <: String](
    d1: Var[Type.Size[I1]],
    d2: Var[Type.Vec[I1, Type.Size[I2]]]
  )(implicit b: Builder): Var[Type.Vec[I1, Type.Vec[I2, T]]] = {
    b.setIndex(id, Seq(d1.id.asIndex, d2.id.asIndex))
    new Var.Simple(id, Vec(d1.id.asIndex, Vec(d2.id.asIndex, varType)))
  }

  def ~(g: Generator[T])(implicit b: Builder): Unit =
    b.setGenerator(id, g)
}
object Var {
  import Type.{ Vec, Category, Size }

  class Simple[T <: Type](override val id: VarID, override val varType: T) extends Var[T] {
    override def deps = Set()
  }
  class Access[T <: Type](
    override val id: VarID,
    override val deps: Set[VarID],
    override val varType: T
  ) extends Var[T]

  implicit class SizeVar[I <: String](self: Var[Size[I]]) {
    def foreach(f: Var[Category[I]] => Unit): Unit = {
      f(new Var.Simple(self.id, Category(self.varType)))
    }
  }
  implicit class Vec1Ops[I <: String, E <: Type](self: Var[Vec[I, E]]) {
    def apply(i: Var[Category[I]]): Var[E] =
      new Var.Access(self.id, self.deps + i.id, self.varType.elementType)

    def apply(i: Var[Size[I]]): Builder.Incomplete[I :: HNil, E] =
      new Builder.Incomplete(self.id, self.varType.elementType)
  }
  implicit class Vec2Ops[I1 <: String, I2 <: String, E <: Type](self: Var[Vec[I1, Vec[I2, E]]]) extends Vec1Ops[I1, Vec[I2, E]](self) {
    def apply(i1: Var[Category[I1]], i2: Var[Category[I2]]): Var[E] = self(i1)(i2)
  }
}
