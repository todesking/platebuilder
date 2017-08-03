package com.todesking.platebuilder

class DotRenderer(
    subgraph: Boolean = false,
    transpose: Boolean = true,
    html: Boolean = true
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

  private[this] def repr(model: Model, v: VarID): String = {
    val bold = !model.varType(v).isScalar
    val varName = model.repr(v)
    s"${if (bold) s"""\\bf{$varName}""" else varName} ∈ ${tpe(model, v)}"
  }
  private[this] def tpe(model: Model, v: VarID): String = {
    val t = model.varType(v)
    val base =
      t.bareType match {
        case Type.Real => "ℝ"
        case Type.Binary => """\{0, 1\}"""
        case Type.Category(s) => s"""\\{1..${s.indexID.str}\\}"""
        case Type.Size(id) => "ℕ"
      }
    val dim =
      if (t.dimension.nonEmpty) s"^{${t.dimension.map(_.str).mkString("×")}}"
      else ""
    base + dim
  }
  private[this] def id(model: Model, v: VarID): String = s"${model.id}_${v.str}"
  private[this] def renderExpr(model: Model, v: VarID, parts: Seq[String], args: Seq[Any]): String = {
    def render(a: Any): String = a match {
      case Var.Simple(id, t) =>
        if (t.isScalar) id.str
        else s"\\bf{${id.str}}"
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
        val name = s"\\bf{${a.id.str}}"
        if (visibleIndices.isEmpty) name
        else s"${name}(${visibleIndices.mkString(", ")})"
    }
    var s = parts(0)
    for (i <- 1 until parts.size) {
      s += render(args(i - 1))
      s += parts(i)
    }
    s
  }
  private[this] def isVisible(model: Model, v: VarID): Boolean =
    model.generator(v) match {
      case Some(Generator.Const(_)) => false
      case _ => true
    }

  private[this] def renderMarkup(s: String): String = {
    val fontSizeMultiplier = 0.75
    def table(s: String*): String =
      tag(
        "TABLE",
        "ALIGN" -> "left",
        "BORDER" -> "0",
        "CELLBORDER" -> "0",
        "CELLPADDING" -> "0",
        "CELLSPACING" -> "0"
      )(s.mkString(""))
    def tr(s: String): String = tag("TR")(s)
    def td(s: String): String = tag("TD")(s)
    def renderUD(u: Option[Markup], d: Option[Markup], prefix: String, fontSize: Double, handlePlain: String => String): String = {
      val size = fontSize * fontSizeMultiplier
      table(
        tr(td(u.map { m => render(m, prefix, size, handlePlain) } getOrElse "&nbsp;")),
        tr(td(d.map { m => render(m, prefix, size, handlePlain) } getOrElse "&nbsp;"))
      )
    }
    def font(size: Double)(s: String): String =
      tag("FONT", "POINT-SIZE" -> f"$size%.2f")(s)
    def tag(name: String, attrs: (String, String)*)(content: String): String = {
      val a = if (attrs.isEmpty) "" else " " + attrs.map { case (k, v) => s"""$k="$v"""" }.mkString(" ")
      s"""<$name$a>$content</$name>"""
    }
    def render(m: Markup, prefix: String, fontSize: Double, handlePlain: String => String): String = m match {
      case Markup.Plain(s) => font(size = fontSize)(prefix + handlePlain(s.replaceAll(" ", "&nbsp;"))) // TODO: escape html
      case Markup.UD(u, d) =>
        renderUD(u, d, "", fontSize, handlePlain)
      case Markup.Group(elms) =>
        table(tr((td(render(elms.head, prefix, fontSize, handlePlain)) +: elms.tail.map { e => td(render(e, "", fontSize, handlePlain)) }).mkString("")))
      case Markup.Bold(elm) =>
        render(elm, prefix, fontSize, s => tag("B")(s))
    }
    val defaultFontSize = 14.0
    val m = Markup.parse(s)
    table(tr(td(render(m, "", defaultFontSize, identity))))
  }

  private[this] def renderVar(model: Model, v: VarID): String = {
    val isSize = model.varType(v).isInstanceOf[Type.Size[_]]
    val (stochastic, definition) = model.generator(v).map {
      case Generator.Const(value) => (false, Seq(value))
      case Generator.Expr(stochastic, deps, parts, args) =>
        (stochastic, Seq(renderExpr(model, v, parts, args)))
      case Generator.Given() =>
        (false, Seq())
    } getOrElse (false, Seq())
    def labelOf(v: VarID): Seq[String] = {
      val l = repr(model, v) +: definition
      if (html) l.map(renderMarkup) else l
    }
    model.observation(v) match {
      case _ if !isVisible(model, v) =>
        ""
      case Observation.Hidden =>
        Dot.record(
          id(model, v),
          transpose = transpose,
          style = "",
          m = true,
          label = labelOf(v),
          html = html
        )
      case Observation.Observed =>
        Dot.record(
          id(model, v),
          transpose = transpose,
          style = "filled",
          m = true,
          label = labelOf(v),
          html = html
        )
      case Observation.Given if !isSize =>
        Dot.record(
          id(model, v),
          transpose = transpose,
          style = "",
          label = labelOf(v),
          html = html
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
