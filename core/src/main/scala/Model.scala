package com.todesking.platebuilder

class Model(
    val id: String,
    val varOrder: Seq[VarID],
    _vars: Map[VarID, Var[_ <: Type]],
    val indices: Map[VarID, Seq[IndexID]],
    _inEdges: Map[VarID, Set[VarID]],
    val generators: Map[VarID, Generator[_ <: Type]],
    val observations: Map[VarID, Observation],
    val descriptions: Map[VarID, String]
) {
  import Model._

  def vars: Seq[VarID] = varOrder
  def varType(id: VarID): Type = _vars(id).varType
  def repr(id: VarID): String = _vars(id) match {
    case Var.Constant(id, value, t) => value.toString
    case v => v.id.str
  }
  def observation(id: VarID): Observation = observations(id)
  def index(id: VarID): Seq[IndexID] = indices.get(id) getOrElse Seq()
  def desc(id: VarID): Option[String] = descriptions.get(id)
  def inEdges(id: VarID): Set[VarID] = _inEdges.get(id) getOrElse Set()
  def generator(id: VarID): Option[Generator[_]] = generators.get(id)

  lazy val grouped: Grouped.Root = {
    def toGrouped(i: Seq[IndexID], v: VarID): Grouped.Root =
      if (i.isEmpty) Grouped.Root(Set(v), Seq())
      else Grouped.Root(Set(), Seq(toGroupChild(i.head, i.tail, v)))

    def toGroupChild(i: IndexID, tail: Seq[IndexID], v: VarID): Grouped.Child = tail match {
      case Seq() => Grouped.Child(i, Set(v), Seq())
      case Seq(h, t @ _*) => Grouped.Child(i, Set(), Seq(toGroupChild(h, t, v)))
    }

    def merge(roots: Seq[Grouped.Root]): Grouped.Root =
      Grouped.Root(roots.flatMap(_.vars).toSet, mergeChildren(roots.flatMap(_.children)))

    def mergeChildren(cs: Seq[Grouped.Child]): Seq[Grouped.Child] =
      cs.groupBy(_.index).map {
        case (i, cs) =>
          Grouped.Child(i, cs.flatMap(_.vars).toSet, mergeChildren(cs.flatMap(_.children)))
      }.toSeq

    merge(vars.toSeq.map { v => toGrouped(index(v), v) })
  }

  def toDot(subgraph: Boolean): String =
    new DotRenderer(subgraph = subgraph).render(this)
}
object Model {
  def define(id: String)(f: Builder => Unit): Model = {
    val b = new Builder(id)
    f(b)
    b.build()
  }

  def toDot(models: Seq[Model]): String = {
    Dot.digraph("models") {
      Seq(
        Seq("""rankdir="TB";""", """charset="UTF-8";"""),
        models.map(_.toDot(true))
      ).flatten.mkString("\n")
    }
  }

  sealed abstract class Grouped
  object Grouped {
    case class Root(vars: Set[VarID], children: Seq[Child]) extends Grouped
    case class Child(index: IndexID, vars: Set[VarID], children: Seq[Child]) extends Grouped
  }

}
