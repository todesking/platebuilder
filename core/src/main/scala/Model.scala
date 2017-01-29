package com.todesking.platebuilder

class Model(
    id: String,
    varOrder: Seq[VarID],
    _vars: Map[VarID, Var[_ <: Type]],
    indices: Map[VarID, Seq[IndexID]],
    _inEdges: Map[VarID, Set[VarID]],
    generators: Map[VarID, Generator[_ <: Type]],
    observations: Map[VarID, Observation],
    descriptions: Map[VarID, String]
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
    def renderExpr(v: VarID, parts: Seq[String], args: Seq[Any]): String = {
      def render(a: Any): String = a match {
        case Var.Simple(id, t) =>
          id.str
        case Var.Constant(id, v, _) =>
          v.toString
        case a @ Var.Access(vec, i) =>
          val path = a.path.map(_.id.asIndex)
          val vPath = index(v)
          val pos = vPath.zipWithIndex.lastIndexWhere {
            case (vid, i) =>
              i < path.size && path(i) == vid
          }
          val visibleIndices =
            if (pos == -1) a.path.map(_.id.str)
            else a.path.drop(pos + 1).map(_.id.str)
          if (visibleIndices.isEmpty) a.id.str
          else s"${a.id.str}(${visibleIndices.mkString(", ")})"
      }
      var s = parts(0)
      for (i <- 1 until parts.size) {
        s += render(args(i - 1))
        s += parts(i)
      }
      s
    }
    def isVisible(v: VarID): Boolean =
      !generator(v).isInstanceOf[Generator.Const[_]]

    def renderVar(v: VarID): String = {
      val isSize = varType(v).isInstanceOf[Type.Size[_]]
      val (stochastic, definition) = generator(v) match {
        case Generator.Const(value) => (false, Seq(value))
        case Generator.Expr(stochastic, deps, parts, args) =>
          (stochastic, Seq(renderExpr(v, parts, args)))
        case Generator.Given() =>
          (false, Seq())
      }
      observation(v) match {
        case _ if !isVisible(v) =>
          ""
        case Observation.Hidden =>
          Dot.record(
            id(v),
            transpose = transpose,
            style = "",
            m = true,
            label = repr(v) +: definition
          )
        case Observation.Observed =>
          Dot.record(
            id(v),
            transpose = transpose,
            style = "filled",
            m = true,
            label = repr(v) +: definition
          )
        case Observation.Given if !isSize =>
          Dot.record(
            id(v),
            transpose = transpose,
            style = "",
            label = repr(v) +: definition
          )
        case Observation.Given =>
          ""
      }
    }
    def renderEdge(from: VarID, to: VarID, stochastic: Boolean): String =
      Dot.edge(id(from), id(to), arrowhead = if (stochastic) "" else "box")
    def renderChild(c: Grouped.Child): String =
      Dot.subGraph(s"cluster_${c.index.str}", label = c.index.str, labeljust = "r", labelloc = "b") {
        (c.vars.map(renderVar) ++ c.children.map(renderChild)).mkString("\n")
      }
    def visibleInEdges(v: VarID): Set[VarID] =
      inEdges(v).flatMap { v2 =>
        if (!isVisible(v2)) Set.empty[VarID]
        else if (index(v).contains(v2.asIndex)) Set.empty[VarID]
        else if (varType(v2).isInstanceOf[Type.Size[_]]) visibleInEdges(v2)
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
          val stochastic = generator(v) match {
            case Generator.Expr(s, _, _, _) => s
            case _ => false
          }
          visibleInEdges(v).map { v2 => renderEdge(v2, v, stochastic) }
        },
        descNode
      ).flatten.mkString("\n")
    if (subgraph) {
      Dot.subGraph(s"cluster_${this.id}", label = this.id)(content)
    } else {
      Dot.digraph(this.id)("""rankdir="TB";charset="UTF-8";""" + content)
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
