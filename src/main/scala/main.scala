package com.todesking.platebuilder

import scala.language.implicitConversions
import scala.language.higherKinds

class Model(
    id: String,
    _vars: Map[VarID, (Type, String)],
    indices: Map[VarID, Seq[IndexID]],
    _inEdges: Map[VarID, Set[VarID]],
    generators: Map[VarID, Generator[_]]
) {
  import Model._

  def vars: Set[VarID] = _vars.keySet
  def varType(id: VarID): Type = _vars(id)._1
  def repr(id: VarID): String = _vars(id)._2
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
    def id(v: VarID): String = s"${this.id}_${v.str}"
    def renderVar(v: VarID): String =
      generator(v) match {
        case Generator.Given(desc) if !varType(v).isInstanceOf[Type.Size[_]] =>
          Dot.node(
            id(v),
            shape = "record",
            label = s"${repr(v)}${desc.fold("") { c => "|" + c }}"
          )
        case Generator.Given(desc) => ""
        case Generator.Sampled(desc, deps) =>
          Dot.node(
            id(v),
            shape = "Mrecord",
            label = s"${repr(v)}${desc.fold("") { c => "|" + c }}"
          )
        case Generator.Observed(desc) =>
          Dot.node(
            id(v),
            shape = "Mrecord",
            style = "filled",
            label = s"${repr(v)}${desc.fold("") { c => "|" + c }}"
          )
        case Generator.Computed(desc, deps) =>
          Dot.node(
            id(v),
            style = "dotted",
            shape = "Mrecord",
            label = s"${repr(v)}${desc.fold("") { c => "|" + c }}"
          )
      }
    def renderEdge(from: VarID, to: VarID): String =
      Dot.edge(id(from), id(to))
    def renderChild(c: Grouped.Child): String =
      Dot.subGraph(s"cluster_${c.index.str}", label = c.index.str) {
        (c.vars.map(renderVar) ++ c.children.map(renderChild)).mkString("\n")
      }
    def visibleInEdges(v: VarID): Set[VarID] =
      inEdges(v).flatMap { v2 =>
        if (varType(v2).isInstanceOf[Type.Size[_]]) visibleInEdges(v2)
        else Set(v2)
      }
    val content =
      Seq(
        grouped.vars.map(renderVar),
        grouped.children.map(renderChild),
        vars.flatMap { v =>
          visibleInEdges(v).map { v2 => renderEdge(v2, v) }
        }
      ).flatten.mkString("\n")
    if (subgraph) {
      Dot.subGraph(s"cluster_${this.id}", label = this.id)(content)
    } else {
      Dot.digraph(this.id)("""rankdir=TB;charset="UTF-8";"""" + content)
    }
  }
}
object Model {
  def define(id: String)(f: Ctx => Unit): Model = {
    val ctx = new Ctx(id)
    f(ctx)
    ctx.build()
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

object Main {
  val Unigram = Model.define("Unigram") { implicit ctx =>
    import ctx.dsl._
    val V = size("V") // num of vocabularies
    val D = size("D") // num of documens
    val N = size("N") * D // N(d): num of words in document d

    // hyperparameters
    val beta = given("beta").vec(V)

    // variables
    val phi = hidden("phi").vec(V)
    val w = observed("w").category(V) * (D, N)

    phi ~ dirichlet(beta)

    for (d <- D) {
      for (n <- N(d)) {
        w(d, n) ~ multinominal(phi)
      }
    }
  }

  val MixtureOfUnigrams = Model.define("MixtureOfUnigrams") { implicit ctx =>
    import ctx.dsl._
    val K = size("K") // num of topics
    val V = size("V") // num of vocabularies
    val D = size("D") // num of documens
    val N = size("N") * D // N(d): num of words in document d

    // hyperparameters
    val alpha = given("alpha").vec(K)
    val beta = given("beta").vec(V)

    // variables
    val phi = hidden("phi").vec(V) * K
    val theta = hidden("theta").vec(K)
    val z = hidden("z").category(K) * D
    val w = observed("w").category(V) * (D, N)

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    theta ~ dirichlet(alpha)

    for (d <- D) {
      z(d) ~ multinominal(theta)
      for (n <- N(d)) {
        w(d, n) ~ multinominal(phi(z(d)))
      }
    }
  }

  val LDA = Model.define("LDA") { implicit ctx =>
    import ctx.dsl._
    val K = size("K") // num of topics
    val V = size("V") // num of vocabularies
    val D = size("D") // num of documens
    val N = size("N") * D // N(d): num of words in document d

    // hyperparameters
    val alpha = given("alpha").vec(K)
    val beta = given("beta").vec(V)

    // variables
    val phi = hidden("phi").vec(V) * K
    val theta = hidden("theta").vec(K) * D
    val z = hidden("z").category(K) * (D, N)
    val w = observed("w").category(V) * (D, N)

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    for (d <- D) {
      theta(d) ~ dirichlet(alpha)
      for (n <- N(d)) {
        z(d, n) ~ multinominal(theta(d))
        w(d, n) ~ multinominal(phi(z(d, n)))
      }
    }
  }

  val PLDA = Model.define("PLDA") { implicit ctx =>
    import ctx.dsl._
    val K = size("K") // num of topics
    val V = size("V") // num of vocabularies
    val D = size("D") // num of documens
    val N = size("N") * D // N(d): num of words in document d
    val L = size("L") // num of labels
    val K_L = size("K_L") * L // K_L(l): num of topics assigned to label l

    val Kd = computed("Kd").size * D // num of labels assigned to document d(=|Lambda(d)|)

    // hyperparameters
    val alpha = given("alpha", "α").R
    val beta = given("beta", "β").vec(V)

    val alphaL = computed("alphaL", "α_L").vec(Kd(D)) * D
    val alphaT = computed("alphaT", "α_T").vec(K) * D

    // variables
    val phi = hidden("phi", "φ").vec(V) * K
    val theta = hidden("theta", "θ").vec(K) * (D, Kd) // theta(d, l): topic distribution of document d and label l
    val Lambda = observed("Lambda", "Λ").vec(L) * D // Set of labels in document d as L-dimensional binary vector
    val psi = hidden("psi", "ψ").vec(Kd(D)) * D
    val z = hidden("z").category(K) * (D, N)
    val w = observed("w").category(V) * (D, N)
    val l = observed("l").category(Kd(D)) * (D, N)

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    for (d <- D) {
      Kd(d) ~ compute("|Λ|", Lambda(d))
      alphaT(d) ~ compute(alpha, Lambda(d))
      for (j <- Kd(d)) {
        theta(d, j) ~ dirichlet(alphaT(d))
      }

      alphaL(d) ~ compute(alpha, Lambda(d), K_L) // alphaL(d, j) = if j in lambda(d) then alpha * Kl(j) else 0
      psi(d) ~ dirichlet(alphaL(d))

      for (n <- N(d)) {
        l(d, n) ~ multinominal(psi(d))
        z(d, n) ~ multinominal(theta(d, l(d, n)))
        w(d, n) ~ multinominal(phi(z(d, n)))
      }
    }
  }

  val models = Seq(Unigram, MixtureOfUnigrams, LDA, PLDA)

  def main(args: Array[String]): Unit = {
    println(Model.toDot(models))
  }
}
