package com.todesking.platebuilder

import scala.language.implicitConversions
import scala.language.higherKinds

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

class Model(
    id: String,
    _vars: Map[Model.VarID, (Model.Type, String)],
    indices: Map[Model.VarID, Seq[Model.IndexID]],
    _inEdges: Map[Model.VarID, Set[Model.VarID]],
    generators: Map[Model.VarID, Model.Generator[_, _]]
) {
  import Model._

  def vars: Set[VarID] = _vars.keySet
  def varType(id: VarID): Type = _vars(id)._1
  def repr(id: VarID): String = _vars(id)._2
  def index(id: VarID): Seq[IndexID] = indices.get(id) getOrElse Seq()
  def inEdges(id: VarID): Set[VarID] = _inEdges.get(id) getOrElse Set()
  def generator(id: VarID): Generator[_, _] = generators(id)

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
        case Generator.Given(desc) if varType(v) != Type.Size =>
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
        if (varType(v2) == Type.Size) visibleInEdges(v2)
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

  class Projection[I1, I2]
  object Projection {
    implicit def self[I1] = new Projection[I1, I1]
  }

  case class VarID(str: String) {
    def asIndex: IndexID = IndexID(str)
  }
  case class IndexID(str: String)

  class Indexable[ID <: String](val id: IndexID, val deps: Set[VarID])
  object Indexable {
    implicit def cat2indexable[ID <: String](v: Var[_, Dim.Nil, Type.Category[ID]]): Indexable[ID] =
      new Indexable(v.id.asIndex, v.deps)
  }

  sealed abstract class Type
  object Type {
    type Real = Real.type
    case object Real extends Type
    type Size = Size.type
    case object Size extends Type
    case class Category[A <: String](catID: VarID) extends Type
  }

  abstract class Var[ID <: String, D <: Dim, T <: Type] {
    def id: VarID
    def deps: Set[VarID]

    protected def unsafeGet(i: IndexID, deps: Set[VarID]): Var[ID, D#Tail, T] =
      new Var.Access(this.deps ++ deps + id, id)

    def foreach(f: Indexable[ID] => Unit)(implicit evD: D =:= Dim.Nil, evT: T =:= Type.Size): Unit =
      f(new Indexable(id.asIndex, deps + id))

    def *[I <: String](
      dim: Var[I, Dim.Nil, Type.Size]
    )(implicit ctx: Ctx): Var[ID, Dim.Succ[I, D], T] = {
      ctx.setIndex(id, Seq(dim.id.asIndex))
      new Var.Simple(id)
    }

    def *[I1 <: String, I2 <: String](
      dim1: Var[I1, Dim.Nil, Type.Size],
      dim2: Var[I2, Dim.Succ[I1, Dim.Nil], Type.Size]
    )(implicit ctx: Ctx): Var[ID, Dim.Succ[I1, Dim.Succ[I2, D]], T] = {
      ctx.setIndex(id, Seq(dim1, dim2).map(_.id.asIndex))
      new Var.Simple(id)
    }

    def ~(g: Generator[D, T])(implicit ctx: Ctx): Unit =
      ctx.setGenerator(id, g)

    def apply[I <: String](i: Var[I, Dim.Nil, Type.Size])(implicit ev: D#Index =:= I): Var[ID, D#Tail, T] =
      unsafeGet(i.id.asIndex, i.deps)

    def apply[I <: String](i: Indexable[I])(implicit ev: D#Index =:= I): Var[ID, D#Tail, T] =
      unsafeGet(i.id, i.deps)

    def apply[I1 <: String, I2 <: String](i1: Indexable[I1], i2: Indexable[I2])(
      implicit
      ev1: D#Index =:= I1, ev2: D#Tail#Index =:= I2
    ): Var[ID, D#Tail#Tail, T] =
      unsafeGet(i1.id, i1.deps).unsafeGet(i2.id, i2.deps)
  }
  object Var {
    class Simple[ID <: String, D <: Dim, T <: Type](override val id: VarID) extends Var[ID, D, T] {
      override def deps = Set()
    }
    class Access[ID <: String, D <: Dim, T <: Type](override val deps: Set[VarID], override val id: VarID) extends Var[ID, D, T]
  }
  abstract class Generator[D <: Dim, T <: Type] {
    def dependencies: Set[VarID]
    def description: Option[String]
  }
  object Generator {
    case class Given[D <: Dim, T <: Type](override val description: Option[String]) extends Generator[D, T] {
      override def dependencies = Set()
    }
    case class Observed[D <: Dim, T <: Type](override val description: Option[String]) extends Generator[D, T] {
      override def dependencies = Set()
    }
    case class Sampled[D <: Dim, T <: Type](override val description: Option[String], override val dependencies: Set[VarID]) extends Generator[D, T]
    case class Computed[D <: Dim, T <: Type](override val description: Option[String], override val dependencies: Set[VarID]) extends Generator[D, T]
  }

  sealed abstract class Dim {
    type Index <: String
    type Tail <: Dim
    type Append[I <: String] <: Dim
  }
  object Dim {
    case class Nil() extends Dim {
      override type Append[I <: String] = Succ[I, Nil]
    }
    case class Succ[A <: String, B <: Dim]() extends Dim {
      override type Index = A
      override type Tail = B
      override type Append[I <: String] = Succ[A, B#Append[I]]
    }
  }

  class VarDef[ID <: String](
      idStr: String,
      ctx: Ctx, generator: Option[Generator[_ <: Dim, _ <: Type]],
      repr: Option[String]
  ) {
    private[this] val id = new VarID(idStr)
    private[this] def register[A <: String, B <: Dim, C <: Type](v: Var[A, B, C], t: C): Var[A, B, C] = {
      ctx.registerVar(v.id, t, repr)
      generator.foreach { g =>
        ctx.setGenerator(v.id, g)
      }
      v
    }

    def size: Var[ID, Dim.Nil, Type.Size] =
      register(new Var.Simple(id), Type.Size)

    def vec[I <: String](dim: Var[I, Dim.Nil, Type.Size]): Var[ID, Dim.Succ[I, Dim.Nil], Type.Real] =
      register(new Var.Simple(id), Type.Real)

    def category[I <: String, D <: Dim](size: Var[I, D, Type.Size]): Var[ID, D, Type.Category[I]] =
      register(new Var.Simple(id), new Type.Category(size.id))

    def R: Var[ID, Dim.Nil, Type.Real] =
      register(new Var.Simple(id), Type.Real)
  }

  class Ctx(id: String) { self =>
    import scala.collection.mutable

    private[this] var vars: Map[VarID, (Type, String)] = Map()
    private[this] var indices: Map[VarID, Seq[IndexID]] = Map()
    private[this] val inEdges: mutable.MultiMap[VarID, VarID] =
      new mutable.HashMap[VarID, mutable.Set[VarID]] with mutable.MultiMap[VarID, VarID]
    private[this] var generators: Map[VarID, Generator[_, _]] = Map()

    def build(): Model = new Model(
      id,
      vars,
      indices,
      inEdges.toMap.mapValues(_.toSet),
      generators
    )

    def registerVar(id: VarID, varType: Type, repr: Option[String]): Unit = {
      vars += (id -> (varType -> repr.getOrElse(id.str)))
    }
    def setIndex(id: VarID, index: Seq[IndexID]): Unit = {
      indices += (id -> index)
    }
    def setGenerator(id: VarID, g: Generator[_ <: Dim, _ <: Type]): Unit = {
      g.dependencies.foreach { d =>
        inEdges.addBinding(id, d)
      }
      generators += (id -> g)
    }

    object dsl {
      private[this] def opt(s: String): Option[String] = if (s.isEmpty) None else Some(s)

      def size(id: String, repr: String = ""): Var[id.type, Dim.Nil, Type.Size] =
        given(id, repr).size

      def compute[D <: Dim, T <: Type](dependencies: Var[_, _, _]*): Generator.Computed[D, T] =
        new Generator.Computed(None, dependencies.map(_.id).toSet ++ dependencies.flatMap(_.deps))

      def compute[D <: Dim, T <: Type](desc: String, dependencies: Var[_, _, _]*): Generator.Computed[D, T] =
        new Generator.Computed(Some(desc), dependencies.map(_.id).toSet ++ dependencies.flatMap(_.deps))

      def given(id: String, repr: String = ""): VarDef[id.type] =
        new VarDef[id.type](id, self, Some(new Generator.Given(None)), opt(repr))

      def observed(id: String, repr: String = ""): VarDef[id.type] =
        new VarDef[id.type](id, self, Some(new Generator.Observed(None)), opt(repr))

      def hidden(id: String, repr: String = ""): VarDef[id.type] =
        new VarDef[id.type](id, self, None, opt(repr))

      def computed(id: String, repr: String = ""): VarDef[id.type] =
        new VarDef[id.type](id, self, None, opt(repr))

      def dirichlet[I <: String](param: Var[_, Dim.Succ[I, Dim.Nil], Type.Real]): Generator.Sampled[Dim.Succ[I, Dim.Nil], Type.Real] =
        new Generator.Sampled(Some(s"Dirichlet(${param.id.str})"), param.deps + param.id)

      def multinominal[I <: String](param: Var[_, Dim.Succ[I, Dim.Nil], Type.Real]): Generator.Sampled[Dim.Nil, Type.Category[I]] =
        new Generator.Sampled(Some(s"Mult(${param.id.str})"), param.deps + param.id)
    }
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
