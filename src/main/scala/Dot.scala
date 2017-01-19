package com.todesking.platebuilder

object Dot {
  private[this] def attr(name: String, value: String): String =
    if (value.isEmpty) "" else s"""$name="$value""""

  def escape(s: String): String =
    s.flatMap { c =>
      if (Character.UnicodeBlock.of(c) == Character.UnicodeBlock.BASIC_LATIN) {
        c match {
          case '{' | '}' => s"&#${c.toInt};"
          case _ => c.toString
        }
      } else {
        s"&#${c.toInt};"
      }
    }

  def escapeRecord(s: String): String =
    s.replaceAll("\\|", "&#124;")

  def node(id: String, shape: String = "ellipse", style: String = "", label: String = ""): String =
    s"""${id}[shape=${shape} ${attr("label", escape(label))} ${attr("style", style)}];"""

  def record(id: String, m: Boolean = false, style: String = "", label: Seq[String] = Seq()): String =
    node(id, shape = if (m) "Mrecord" else "record", style = style, label = label.map(escapeRecord).mkString("|"))

  def edge(from: String, to: String): String =
    s"${from} -> ${to};"

  def subGraph(id: String, label: String = "")(f: => String): String =
    s"""subgraph $id {
       |${attr("label", escape(label))};
       |${f}
       |}""".stripMargin

  def digraph(id: String)(f: => String): String =
    s"""digraph $id {
       |${f}
       |}""".stripMargin
}

