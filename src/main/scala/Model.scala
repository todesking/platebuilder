package com.todesking.platebuilder

class Model(
    id: String,
    varOrder: Seq[VarID],
    _vars: Map[VarID, (Type, String, Boolean)],
    indices: Map[VarID, Seq[IndexID]],
    _inEdges: Map[VarID, Set[VarID]],
    generators: Map[VarID, Generator[_]],
    descriptions: Map[VarID, String]
) {
  import Model._

  def vars: Seq[VarID] = varOrder
  def varType(id: VarID): Type = _vars(id)._1
  def repr(id: VarID): String = _vars(id)._2
  def observed(id: VarID): Boolean = _vars(id)._3
  def index(id: VarID): Seq[IndexID] = indices.get(id) getOrElse Seq()
  def inEdges(id: VarID): Set[VarID] = _inEdges.get(id) getOrElse Set()
  def generator(id: VarID): Generator[_] = generators(id)

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

  def toDot(subgraph: Boolean = false): String = {
    def repr(v: VarID): String = s"${this.repr(v)} ∈ ${tpe(v)}"
    def tpe(v: VarID): String = {
      val t = varType(v)
      val base =
        t.bareType match {
          case Type.Real => "R"
          case Type.Binary => "{0, 1}"
          case Type.Category(s) => s"{1..${s.indexID.str}}"
          case Type.Size(id) => "N"
        }
      val dim =
        if (t.dimension.nonEmpty) s" ^ ${t.dimension.map(_.str).mkString("×")}"
        else ""
      base + dim
    }
    def id(v: VarID): String = s"${this.id}_${v.str}"
    val transpose = true
    def renderVar(v: VarID): String = {
      generator(v) match {
        case Generator.Given(desc) if !varType(v).isInstanceOf[Type.Size[_]] =>
          Dot.record(
            id(v),
            transpose = transpose,
            style = if (observed(v)) "filled" else "",
            label = Seq(repr(v)) ++ desc.toSeq
          )
        case Generator.Given(desc) => ""
        case Generator.Sampled(desc, deps) =>
          Dot.record(
            id(v),
            transpose = transpose,
            style = if (observed(v)) "filled" else "",
            m = true,
            label = Seq(repr(v)) ++ desc.toSeq
          )
        case Generator.Computed(desc, deps) =>
          Dot.record(
            id(v),
            transpose = transpose,
            style = if (observed(v)) "filled,dotted" else "dotted",
            m = true,
            label = Seq(repr(v)) ++ desc.toSeq
          )
      }
    }
    def renderEdge(from: VarID, to: VarID): String =
      Dot.edge(id(from), id(to))
    def renderChild(c: Grouped.Child): String =
      Dot.subGraph(s"cluster_${c.index.str}", label = c.index.str, labeljust = "r", labelloc = "b") {
        (c.vars.map(renderVar) ++ c.children.map(renderChild)).mkString("\n")
      }
    def visibleInEdges(v: VarID): Set[VarID] =
      inEdges(v).flatMap { v2 =>
        if (varType(v2).isInstanceOf[Type.Size[_]]) visibleInEdges(v2)
        else Set(v2)
      }
    val descs =
      vars.flatMap { v =>
        descriptions.get(v).map { d =>
          s"${v.str}: ${d}"
        }
      }
    val descNode =
      if (descs.isEmpty) Seq()
      else Seq(Dot.node(
        id = s"${this.id}_NOTE",
        shape = "note",
        label = descs.mkString("", "\\l", "\\l")
      ))
    val content =
      Seq(
        grouped.vars.map(renderVar),
        grouped.children.map(renderChild),
        vars.flatMap { v =>
          visibleInEdges(v).map { v2 => renderEdge(v2, v) }
        },
        descNode
      ).flatten.mkString("\n")
    if (subgraph) {
      Dot.subGraph(s"cluster_${this.id}", label = this.id)(content)
    } else {
      Dot.digraph(this.id)("""rankdir=TB;charset="UTF-8";"""" + content)
    }
  }
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
