package com.todesking.platebuilder

import scala.util.parsing.combinator.RegexParsers

sealed abstract class Markup {
}
object Markup {
  case class Plain(text: String) extends Markup
  case class Sup(markup: Markup) extends Markup
  case class Sub(markup: Markup) extends Markup
  case class Group(elements: Seq[Markup]) extends Markup

  def parse(s: String): Markup = Parser.parse(s)

  object Parser extends RegexParsers {
    def parse(s: String): Markup = Group(parse(markup, s).get)

    override def skipWhitespace = false

    def markup: Parser[Seq[Markup]] = rep(single)
    def single: Parser[Markup] = plain | sup | sub | group
    def group: Parser[Group] = "{" ~> (rep(single) ^^ Group.apply) <~ "}"
    def plain: Parser[Plain] = rep1(plainChar) ^^ { chars => Plain(chars.mkString("")) }
    def plainChar: Parser[Char] = ("""\\.""".r ^^ { s => s.charAt(1) }) | ("""[^^_{}]""".r ^^ { s => s.charAt(0) })
    def sup: Parser[Sup] = "^" ~> (plainChar ^^ { c => Plain(c.toString) } | group) ^^ Sup.apply
    def sub: Parser[Sub] = "_" ~> (plainChar ^^ { c => Plain(c.toString) } | group) ^^ Sub.apply
  }
}
