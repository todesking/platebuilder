package com.todesking.platebuilder

import scala.util.parsing.combinator.RegexParsers

sealed abstract class Markup {
}
object Markup {
  case class Plain(text: String) extends Markup
  case class UD(upper: Option[Markup], downer: Option[Markup]) extends Markup
  case class Group(elements: Seq[Markup]) extends Markup

  def parse(s: String): Group = Parser.parse(s)

  object Parser extends RegexParsers {
    def parse(s: String): Group = Group(parse(markup, s).get)

    override def skipWhitespace = false

    def markup: Parser[Seq[Markup]] = rep(single)
    def single: Parser[Markup] = plain | sup | sub | group
    def group: Parser[Group] = "{" ~> (rep(single) ^^ Group.apply) <~ "}"
    def plain: Parser[Plain] = rep1(plainChar) ^^ { chars => Plain(chars.mkString("")) }
    def plainChar: Parser[Char] = ("""\\.""".r ^^ { s => s.charAt(1) }) | ("""[^^_{}]""".r ^^ { s => s.charAt(0) })
    def sup: Parser[Markup] = sup0 ~ sub0.? ^^ { case u ~ d => UD(Some(u), d) }
    def sub: Parser[Markup] = sub0 ~ sup0.? ^^ { case d ~ u => UD(u, Some(d)) }
    def sup0: Parser[Markup] = "^" ~> (plainChar ^^ { c => Plain(c.toString) } | group)
    def sub0: Parser[Markup] = "_" ~> (plainChar ^^ { c => Plain(c.toString) } | group)
  }
}
