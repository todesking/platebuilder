package com.todesking.platebuilder

object Dot {
  private[this] def attr(name: String, value: String): String =
    if (value.isEmpty) "" else s"""$name="$value""""

  def escape(s: String): String =
    s.flatMap { c =>
      if (Character.UnicodeBlock.of(c) == Character.UnicodeBlock.BASIC_LATIN) {
        c.toString
      } else {
        s"&#${c.toInt};"
      }
    }

  def node(id: String, shape: String = "ellipse", style: String = "", label: String = ""): String =
    s"""${id}[shape=${shape} ${attr("label", escape(label))} ${attr("style", style)}];"""

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

