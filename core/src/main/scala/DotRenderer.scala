package com.todesking.platebuilder

class DotRenderer(
    subgraph: Boolean = false,
    transpose: Boolean = true
) {
  def render(model: Model): String = {
    val descs =
      model.vars.flatMap { v =>
        model.descriptions.get(v).map { d =>
          s"${v.str}: ${d}"
        }
      }
    val descNode =
      if (descs.isEmpty) Seq()
      else Seq(Dot.node(
        id = s"${model.id}_NOTE",
        shape = "note",
        label = descs.mkString("", "\\l", "\\l")
      ))
    val content =
      Seq(
        model.grouped.vars.map(renderVar(model, _)),
        model.grouped.children.map(renderChild(model, _)),
        model.vars.flatMap { v =>
          val stochastic = model.generator(v) match {
            case Some(Generator.Expr(s, _, _, _)) => s
            case _ => false
          }
          visibleInEdges(model, v).map { v2 => renderEdge(model, v2, v, stochastic) }
        },
        descNode
      ).flatten.mkString("\n")
    if (subgraph) {
      Dot.subGraph(s"cluster_${model.id}", label = model.id)(content)
    } else {
      Dot.digraph(model.id)("""rankdir="TB";charset="UTF-8";""" + content)
    }
  }

  private[this] def repr(model: Model, v: VarID): String = s"${model.repr(v)} ∈ ${tpe(model, v)}"
  private[this] def tpe(model: Model, v: VarID): String = {
    val t = model.varType(v)
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
  private[this] def id(model: Model, v: VarID): String = s"${model.id}_${v.str}"
  private[this] def renderExpr(model: Model, v: VarID, parts: Seq[String], args: Seq[Any]): String = {
    def render(a: Any): String = a match {
      case Var.Simple(id, t) =>
        id.str
      case Var.Constant(id, v, _) =>
        v.toString
      case a @ Var.Access(vec, i) =>
        val path = a.path.map(_.id.asIndex)
        val vPath = model.index(v)
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
  private[this] def isVisible(model: Model, v: VarID): Boolean =
    !model.generator(v).isInstanceOf[Generator.Const[_]]

  private[this] def renderVar(model: Model, v: VarID): String = {
    val isSize = model.varType(v).isInstanceOf[Type.Size[_]]
    val (stochastic, definition) = model.generator(v).map {
      case Generator.Const(value) => (false, Seq(value))
      case Generator.Expr(stochastic, deps, parts, args) =>
        (stochastic, Seq(renderExpr(model, v, parts, args)))
      case Generator.Given() =>
        (false, Seq())
    } getOrElse (false, Seq())
    model.observation(v) match {
      case _ if !isVisible(model, v) =>
        ""
      case Observation.Hidden =>
        Dot.record(
          id(model, v),
          transpose = transpose,
          style = "",
          m = true,
          label = repr(model, v) +: definition
        )
      case Observation.Observed =>
        Dot.record(
          id(model, v),
          transpose = transpose,
          style = "filled",
          m = true,
          label = repr(model, v) +: definition
        )
      case Observation.Given if !isSize =>
        Dot.record(
          id(model, v),
          transpose = transpose,
          style = "",
          label = repr(model, v) +: definition
        )
      case Observation.Given =>
        ""
    }
  }
  private[this] def renderEdge(model: Model, from: VarID, to: VarID, stochastic: Boolean): String =
    Dot.edge(id(model, from), id(model, to))
  private[this] def renderChild(model: Model, c: Model.Grouped.Child): String =
    Dot.subGraph(s"cluster_${c.index.str}", label = c.index.str, labeljust = "r", labelloc = "b") {
      (c.vars.map(renderVar(model, _)) ++ c.children.map(renderChild(model, _))).mkString("\n")
    }
  private[this] def visibleInEdges(model: Model, v: VarID): Set[VarID] =
    model.inEdges(v).flatMap { v2 =>
      if (!isVisible(model, v2)) Set.empty[VarID]
      else if (model.index(v).contains(v2.asIndex)) Set.empty[VarID]
      else if (model.varType(v2).isInstanceOf[Type.Size[_]]) visibleInEdges(model, v2)
      else Set(v2)
    }
}
