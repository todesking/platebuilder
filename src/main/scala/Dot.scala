package com.todesking.platebuilder

object Dot {
  private[this] def attr(name: String, value: String): String =
    if (value.isEmpty) "" else s"""$name="$value""""
  private[this] def attrL(name: String, value: String): String =
    if (value.isEmpty) "" else s"""$name="$value";"""

  def escape(s: String): String =
    s.flatMap { c =>
      if (Character.UnicodeBlock.of(c) == Character.UnicodeBlock.BASIC_LATIN) {
        c.toString
      } else {
        s"&#${c.toInt};"
      }
    }
  def idEscape(s: String): String =
    s.flatMap {
      case c if ('a' <= c && c <= 'z') => c.toString
      case c if ('A' <= c && c <= 'Z') => c.toString
      case c if ('0' <= c && c <= '9') => c.toString
      case c @ '_' => c.toString
      case c => s"_${c.toInt}"
    }

  def escapeRecord(s: String): String =
    s.flatMap {
      case c @ ('{' | '}' | '|') => s"&#${c.toInt};"
      case c => c.toString
    }

  def node(id: String, shape: String = "ellipse", style: String = "", label: String = ""): String =
    s"""${idEscape(id)}[shape=${shape} ${attr("label", escape(label))} ${attr("style", style)}];"""

  def record(id: String, m: Boolean = false, style: String = "", label: Seq[String] = Seq(), transpose: Boolean = false): String = {
    val l = label.map(escapeRecord).mkString("|")
    node(id, shape = if (m) "Mrecord" else "record", style = style, label = if (transpose) s"{$l}" else l)
  }

  def edge(from: String, to: String): String =
    s"${idEscape(from)} -> ${idEscape(to)};"

  def subGraph(
    id: String,
    label: String = "",
    labeljust: String = "",
    labelloc: String = ""
  )(f: => String): String =
    s"""subgraph ${idEscape(id)} {
       |${attrL("label", escape(label))}
       |${attrL("labeljust", labeljust)}
       |${attrL("labeltoc", labelloc)}
       |${f}
       |}""".stripMargin

  def digraph(id: String)(f: => String): String =
    s"""digraph ${idEscape(id)} {
       |${f}
       |}""".stripMargin
}

