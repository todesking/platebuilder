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
    generators: Map[Model.VarID, Model.Generator[_]]
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

  class Projection[I1, I2]
  object Projection {
    implicit def self[I1] = new Projection[I1, I1]
  }

  case class VarID(str: String) {
    def asIndex: IndexID = IndexID(str)
  }
  case class IndexID(str: String) // TODO: IndexType(Var|Constant)

  sealed abstract class Type
  object Type {
    type Real = Real.type
    case object Real extends Type
    case class Size[A <: String]() extends Type
    case class Category[A <: String](size: Size[A]) extends Type
    case class Vec[I <: String, A <: Type](elm: A) extends Type {
      def elementType: A = elm
    }
  }

  abstract class Var[T <: Type] {
    import Type.{ Category, Size, Vec }
    def id: VarID
    def varType: T
    def deps: Set[VarID]

    def *[I <: String](dim: Var[Type.Size[I]])(implicit ctx: Ctx): Var[Type.Vec[I, T]] = {
      ctx.setIndex(id, Seq(dim.id.asIndex))
      new Var.Simple(id, Vec(varType))
    }

    def *[I1 <: String, I2 <: String](
      d1: Var[Type.Size[I1]],
      d2: Var[Type.Vec[I1, Type.Size[I2]]]
    )(implicit ctx: Ctx): Var[Type.Vec[I1, Type.Vec[I2, T]]] = {
      ctx.setIndex(id, Seq(d1.id.asIndex, d2.id.asIndex))
      new Var.Simple(id, Vec(Vec(varType)))
    }

    def ~(g: Generator[T])(implicit ctx: Ctx): Unit =
      ctx.setGenerator(id, g)
  }
  object Var {
    import Type.{ Vec, Category, Size }

    class Simple[T <: Type](override val id: VarID, override val varType: T) extends Var[T] {
      override def deps = Set()
    }
    class Access[T <: Type](
      override val id: VarID,
      override val deps: Set[VarID],
      override val varType: T
    ) extends Var[T]

    implicit class SizeVar[I <: String](self: Var[Size[I]]) {
      def foreach(f: Var[Category[I]] => Unit): Unit = {
        f(new Var.Simple(self.id, Category(self.varType)))
      }
    }
    implicit class Vec1Ops[I <: String, E <: Type](self: Var[Vec[I, E]]) {
      def apply(i: Var[Category[I]]): Var[E] =
        new Var.Access(self.id, self.deps + i.id, self.varType.elementType)

      def apply(i: Var[Size[I]]): Incomplete[I :: HNil, E] =
        new Incomplete(self.id, self.varType.elementType)
    }
    implicit class Vec2Ops[I1 <: String, I2 <: String, E <: Type](self: Var[Vec[I1, Vec[I2, E]]]) extends Vec1Ops[I1, Vec[I2, E]](self) {
      def apply(i1: Var[Category[I1]], i2: Var[Category[I2]]): Var[E] = self(i1)(i2)
    }
  }
  sealed abstract class HList
  class HNil extends HList
  class ::[A, B <: HList] extends HList

  class Incomplete[Deps <: HList, E <: Type](val id: VarID, val varType: E) {
    import Type.{ Vec, Size, Category }
    def *[I <: String](dim: Var[Size[I]])(implicit ctx: Ctx, ev: Deps =:= (I :: HNil)): Var[Vec[I, E]] = {
      val v = new Var.Simple(id, varType)
      ctx.registerVar(v, None)
      v * dim
    }

    def *[I1 <: String, I2 <: String](dim1: Var[Size[I1]], dim2: Var[Vec[I1, Size[I2]]])(implicit ctx: Ctx, ev: Deps =:= (I1 :: HNil)): Var[Vec[I1, Vec[I2, E]]] = {
      val v = new Var.Simple(id, varType)
      ctx.registerVar(v, None)
      v * (dim1, dim2)
    }
  }

  abstract class Generator[T <: Type] {
    def dependencies: Set[VarID]
    def description: Option[String]
  }
  object Generator {
    case class Given[T <: Type](override val description: Option[String]) extends Generator[T] {
      override def dependencies = Set()
    }
    case class Observed[T <: Type](override val description: Option[String]) extends Generator[T] {
      override def dependencies = Set()
    }
    case class Sampled[T <: Type](override val description: Option[String], override val dependencies: Set[VarID]) extends Generator[T]
    case class Computed[T <: Type](override val description: Option[String], override val dependencies: Set[VarID]) extends Generator[T]
  }

  class VarDef[ID <: String](
      idStr: String,
      ctx: Ctx, generator: Option[Generator[_ <: Type]],
      repr: Option[String]
  ) {
    private[this] val id = new VarID(idStr)
    private[this] def register[T <: Type](v: Var[T]): Var[T] = {
      ctx.registerVar(v, repr)
      generator.foreach { g =>
        ctx.setGenerator(v.id, g)
      }
      v
    }

    def size: Var[Type.Size[ID]] =
      register(new Var.Simple(id, Type.Size()))

    def vec[I <: String](dim: Var[Type.Size[I]]): Var[Type.Vec[I, Type.Real]] =
      register(new Var.Simple(id, Type.Vec(Type.Real)))

    def vec[I <: String, II <: HList](dim: Incomplete[II, Type.Size[I]]): Incomplete[II, Type.Vec[I, Type.Real]] =
      new Incomplete(id, Type.Vec(Type.Real))

    def category[I <: String](size: Var[Type.Size[I]]): Var[Type.Category[I]] =
      register(new Var.Simple(id, Type.Category(size.varType)))

    def category[I <: String, II <: HList](size: Incomplete[II, Type.Size[I]]): Incomplete[II, Type.Category[I]] =
      new Incomplete(id, Type.Category(size.varType))

    def R: Var[Type.Real] =
      register(new Var.Simple(id, Type.Real))
  }

  class Ctx(id: String) { self =>
    import scala.collection.mutable

    private[this] var vars: Map[VarID, (Type, String)] = Map()
    private[this] var indices: Map[VarID, Seq[IndexID]] = Map()
    private[this] val inEdges: mutable.MultiMap[VarID, VarID] =
      new mutable.HashMap[VarID, mutable.Set[VarID]] with mutable.MultiMap[VarID, VarID]
    private[this] var generators: Map[VarID, Generator[_]] = Map()

    def build(): Model = {
      val missingGenerators = vars.keys.filterNot { id => generators.contains(id) }
      if (missingGenerators.nonEmpty) {
        throw new IllegalStateException(s"Generator undefined: ${missingGenerators.map(_.str).mkString(", ")}")
      }
      val undefinedVars = (
        indices.keySet ++ indices.values.flatten.map(id => VarID(id.str)).toSet ++ inEdges.keySet ++ inEdges.values.flatten.toSet ++ generators.keySet
      ).filterNot { id => vars.contains(id) }
      if (undefinedVars.nonEmpty) {
        throw new IllegalStateException(s"Unknown variables: ${undefinedVars.map(_.str).mkString(", ")}")
      }
      new Model(
        id,
        vars,
        indices,
        inEdges.toMap.mapValues(_.toSet),
        generators
      )
    }

    def registerVar(v: Var[_ <: Type], repr: Option[String]): Unit = {
      vars += (v.id -> (v.varType -> v.id.str))
    }
    def setIndex(id: VarID, index: Seq[IndexID]): Unit = {
      indices += (id -> index)
    }
    def setGenerator(id: VarID, g: Generator[_ <: Type]): Unit = {
      g.dependencies.foreach { d =>
        inEdges.addBinding(id, d)
      }
      generators += (id -> g)
    }

    object dsl {
      private[this] def opt(s: String): Option[String] = if (s.isEmpty) None else Some(s)

      def size(id: String, repr: String = ""): Var[Type.Size[id.type]] =
        given(id, repr).size

      def compute[T <: Type](dependencies: Var[_]*): Generator.Computed[T] =
        new Generator.Computed(None, dependencies.map(_.id).toSet ++ dependencies.flatMap(_.deps))

      def compute[T <: Type](desc: String, dependencies: Var[_]*): Generator.Computed[T] =
        new Generator.Computed(Some(desc), dependencies.map(_.id).toSet ++ dependencies.flatMap(_.deps))

      def given(id: String, repr: String = ""): VarDef[id.type] =
        new VarDef[id.type](id, self, Some(new Generator.Given(None)), opt(repr))

      def observed(id: String, repr: String = ""): VarDef[id.type] =
        new VarDef[id.type](id, self, Some(new Generator.Observed(None)), opt(repr))

      def hidden(id: String, repr: String = ""): VarDef[id.type] =
        new VarDef[id.type](id, self, None, opt(repr))

      def computed(id: String, repr: String = ""): VarDef[id.type] =
        new VarDef[id.type](id, self, None, opt(repr))

      def dirichlet[I <: String](param: Var[Type.Vec[I, Type.Real]]): Generator.Sampled[Type.Vec[I, Type.Real]] =
        new Generator.Sampled(Some(s"Dirichlet(${param.id.str})"), param.deps + param.id)

      def multinominal[I <: String](param: Var[Type.Vec[I, Type.Real]]): Generator.Sampled[Type.Category[I]] =
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
