package com.todesking.platebuilder

abstract class Var[T <: Type] {
  import Type.{ Category, Size, Vec }
  def id: VarID
  def varType: T
  def deps: Set[VarID]

  def *[I <: String](dim: Var[Size[I]])(implicit b: Builder): Var[Vec[I, T]] = {
    b.setIndex(id, Seq(dim.id.asIndex))
    new Var.Simple(id, Vec(dim.id.asIndex, varType))
  }

  def *[I1 <: String, I2 <: String](
    d1: Var[Size[I1]],
    d2: Var[Vec[I1, Size[I2]]]
  )(implicit b: Builder): Var[Vec[I1, Vec[I2, T]]] = {
    b.setIndex(id, Seq(d1.id.asIndex, d2.id.asIndex))
    new Var.Simple(id, Vec(d1.id.asIndex, Vec(d2.id.asIndex, varType)))
  }

  def *[I1 <: String, I2 <: String, I3 <: String](
    d1: Var[Size[I1]],
    d2: Var[Vec[I1, Size[I2]]],
    d3: Var[Vec[I1, Vec[I2, Size[I3]]]]
  )(implicit b: Builder): Var[Vec[I1, Vec[I2, Vec[I3, T]]]] = {
    b.setIndex(id, Seq(d1.id.asIndex, d2.id.asIndex, d3.id.asIndex))
    new Var.Simple(id, Vec(d1.id.asIndex, Vec(d2.id.asIndex, Vec(d3.id.asIndex, varType))))
  }

  def *[I1 <: String, I2 <: String, I3 <: String, I4 <: String](
    d1: Var[Size[I1]],
    d2: Var[Vec[I1, Size[I2]]],
    d3: Var[Vec[I1, Vec[I2, Size[I3]]]],
    d4: Var[Vec[I1, Vec[I2, Vec[I3, Size[I4]]]]]
  )(implicit b: Builder): Var[Vec[I1, Vec[I2, Vec[I3, Vec[I4, T]]]]] = {
    b.setIndex(id, Seq(d1.id.asIndex, d2.id.asIndex, d3.id.asIndex, d4.id.asIndex))
    new Var.Simple(id, Vec(d1.id.asIndex, Vec(d2.id.asIndex, Vec(d3.id.asIndex, Vec(d4.id.asIndex, varType)))))
  }

  def *[I1 <: String, I2 <: String, I3 <: String, I4 <: String, I5 <: String](
    d1: Var[Size[I1]],
    d2: Var[Vec[I1, Size[I2]]],
    d3: Var[Vec[I1, Vec[I2, Size[I3]]]],
    d4: Var[Vec[I1, Vec[I2, Vec[I3, Size[I4]]]]],
    d5: Var[Vec[I1, Vec[I2, Vec[I3, Vec[I4, Size[I5]]]]]]
  )(implicit b: Builder): Var[Vec[I1, Vec[I2, Vec[I3, Vec[I4, Vec[I5, T]]]]]] = {
    b.setIndex(id, Seq(d1.id.asIndex, d2.id.asIndex, d3.id.asIndex, d4.id.asIndex, d5.id.asIndex))
    new Var.Simple(id, Vec(d1.id.asIndex, Vec(d2.id.asIndex, Vec(d3.id.asIndex, Vec(d4.id.asIndex, Vec(d5.id.asIndex, varType))))))
  }

  def ~(g: Generator[T])(implicit b: Builder): Unit =
    b.setGenerator(id, g)
}
object Var {
  import Type.{ Vec, Category, Size }

  case class Simple[T <: Type](override val id: VarID, override val varType: T) extends Var[T] {
    override def deps = Set()
  }
  case class Constant[T <: Type](override val id: VarID, value: Any, override val varType: T) extends Var[T] {
    override def deps = Set()
  }
  case class Access[T <: Type, I <: String](
      vec: Var[Vec[I, T]],
      index: Var[Category[I]]
  ) extends Var[T] {
    override def id: VarID = vec.id
    override def varType: T = vec.varType.elementType
    override def deps: Set[VarID] = vec.deps ++ index.deps + index.id
    def path: Seq[Var[Category[_]]] = vec match {
      case Simple(_, _) => Seq(index.asInstanceOf[Var[Category[_]]])
      case a @ Access(v, i) => a.path ++ Seq(index.asInstanceOf[Var[Category[_]]])
    }
  }

  implicit class SizeVar[I <: String](self: Var[Size[I]]) {
    def foreach(f: Var[Category[I]] => Unit): Unit = {
      f(new Var.Simple(self.id, Category(self.varType)))
    }
  }
  implicit class Vec1Ops[I <: String, E <: Type](self: Var[Vec[I, E]]) {
    def apply(i: Var[Category[I]]): Var[E] =
      Var.Access(self, i)

    def apply(i: Var[Size[I]]): Builder.Incomplete[I :: HNil, E] =
      new Builder.Incomplete(self.id, self.varType.elementType, false) // TODO: Use another class
  }
  implicit class Vec2Ops[I1 <: String, I2 <: String, E <: Type](self: Var[Vec[I1, Vec[I2, E]]]) extends Vec1Ops[I1, Vec[I2, E]](self) {
    def apply(i1: Var[Category[I1]], i2: Var[Category[I2]]): Var[E] = self(i1)(i2)
  }
  implicit class Vec3Ops[I1 <: String, I2 <: String, I3 <: String, E <: Type](self: Var[Vec[I1, Vec[I2, Vec[I3, E]]]])
      extends Vec2Ops[I1, I2, Vec[I3, E]](self) {
    def apply(
      i1: Var[Category[I1]],
      i2: Var[Category[I2]],
      i3: Var[Category[I3]]
    ): Var[E] = self(i1)(i2)(i3)
  }
  implicit class Vec4Ops[I1 <: String, I2 <: String, I3 <: String, I4 <: String, E <: Type](self: Var[Vec[I1, Vec[I2, Vec[I3, Vec[I4, E]]]]])
      extends Vec3Ops[I1, I2, I3, Vec[I4, E]](self) {
    def apply(
      i1: Var[Category[I1]],
      i2: Var[Category[I2]],
      i3: Var[Category[I3]],
      i4: Var[Category[I4]]
    ): Var[E] = self(i1)(i2)(i3)(i4)
  }
  implicit class Vec5Ops[I1 <: String, I2 <: String, I3 <: String, I4 <: String, I5 <: String, E <: Type](self: Var[Vec[I1, Vec[I2, Vec[I3, Vec[I4, Vec[I5, E]]]]]])
      extends Vec4Ops[I1, I2, I3, I4, Vec[I5, E]](self) {
    def apply(
      i1: Var[Category[I1]],
      i2: Var[Category[I2]],
      i3: Var[Category[I3]],
      i4: Var[Category[I4]],
      i5: Var[Category[I5]]
    ): Var[E] = self(i1)(i2)(i3)(i4)(i5)
  }
}
