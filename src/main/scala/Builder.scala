package com.todesking.platebuilder

class Builder(id: String) { self =>
  import scala.collection.mutable
  import Builder.VarDef

  private[this] var vars: Map[VarID, Var[_ <: Type]] = Map()
  private[this] var varOrder: Seq[VarID] = Seq()
  private[this] var indices: Map[VarID, Seq[IndexID]] = Map()
  private[this] val inEdges: mutable.MultiMap[VarID, VarID] =
    new mutable.HashMap[VarID, mutable.Set[VarID]] with mutable.MultiMap[VarID, VarID]
  private[this] var generators: Map[VarID, Generator[_ <: Type]] = Map()
  private[this] var descs: Map[VarID, String] = Map()
  private[this] var observations: Map[VarID, Observation] = Map()

  private[this] var _currentIndices: Seq[IndexID] = Seq()

  def build(): Model = {
    val missingGenerators = vars.keys.filterNot { id => generators.contains(id) }
    if (missingGenerators.nonEmpty) {
      throw new IllegalStateException(s"Generator undefined: ${missingGenerators.map(_.str).mkString(", ")}")
    }
    val undefinedVars = (
      indices.keySet ++ indices.values.flatten.map(id => VarID(id.str)).toSet ++ inEdges.keySet ++ inEdges.values.flatten.toSet ++ generators.keySet ++ descs.keySet
    ).filterNot { id => vars.contains(id) }
    if (undefinedVars.nonEmpty) {
      throw new IllegalStateException(s"Unknown variables: ${undefinedVars.map(_.str).mkString(", ")}")
    }
    new Model(
      id,
      varOrder,
      vars,
      indices,
      inEdges.toMap.mapValues(_.toSet),
      generators,
      observations,
      descs.toMap
    )
  }

  def registerVar(v: Var[_ <: Type], desc: Option[String]): Unit = {
    if (!vars.contains(v.id)) {
      varOrder :+= v.id
    }
    vars += (v.id -> v)
    desc.foreach { d =>
      descs += (v.id -> d)
    }
  }

  // TODO: rename to addIndex
  def setIndex(id: VarID, index: Seq[IndexID]): Unit = {
    indices.get(id).fold {
      indices += (id -> index)
    } { is =>
      indices += (id -> (index ++ is))
    }
  }

  def setIndex(id: VarID): Unit = {
    indices += (id -> _currentIndices)
  }

  def withIndex[A](i: IndexID)(f: => A): A = {
    _currentIndices :+= i
    val ret = f
    _currentIndices = _currentIndices.take(_currentIndices.size - 1)
    ret
  }

  def setGenerator(id: VarID, g: Generator[_ <: Type]): Unit = {
    inEdges.remove(id)
    g.dependencies.foreach { d =>
      inEdges.addBinding(id, d)
    }
    generators += (id -> g)
  }

  def setObservation(v: VarID, o: Observation): Unit = {
    observations += (v -> o)
  }

  object dsl {
    private[this] def opt(s: String): Option[String] = if (s.isEmpty) None else Some(s)

    implicit class GeneratorSyntax(sc: StringContext) {
      private[this] def filterVars(args: Seq[Any]): Seq[Var[_]] =
        args.collect { case v: Var[_] => v }

      private[this] def deps(v: Var[_]): Set[VarID] =
        v.deps + v.id

      def stochastic[A <: Type](args: Any*): Generator[A] =
        new Generator.Expr(true, filterVars(args).flatMap(deps).toSet, sc.parts, args)

      def deterministic[A <: Type](args: Any*): Generator[A] =
        new Generator.Expr(false, filterVars(args).flatMap(deps).toSet, sc.parts, args)
    }

    def const(n: Double): Var[Type.Real] = {
      val v = new Var.Constant(VarID(s"constant_R_${n}"), n, Type.Real)
      self.registerVar(v, None)
      self.setObservation(v.id, Observation.Given)
      self.setGenerator(v.id, new Generator.Const(n.toString))
      v
    }

    def size(id: String, desc: String = ""): Var[Type.Size[id.type]] =
      given(id, desc).size

    def given(id: String, desc: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, Some(Observation.Given), opt(desc))

    def observed(id: String, desc: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, Some(Observation.Observed), opt(desc))

    def hidden(id: String, desc: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, Some(Observation.Hidden), opt(desc))

    def computed(id: String, desc: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, Some(Observation.Hidden), opt(desc)) // TODO: inherit observation from its dependencies

    def dirichlet[I <: String](param: Var[Type.Vec[I, Type.Real]]): Generator[Type.Vec[I, Type.Real]] =
      stochastic"Dirichlet($param)"

    def multinominal[I <: String](param: Var[Type.Vec[I, Type.Real]]): Generator[Type.Category[I]] =
      stochastic"Mult($param)"

    def normal(mu: Var[Type.Real], s2: Var[Type.Real]): Generator[Type.Real] =
      stochastic"Normal($mu, $s2)"

    def mapping[A <: String, B <: String](a: Var[Type.Size[A]], b: Var[Type.Size[B]]): Builder.Mapping[A, B] =
      new Builder.Mapping(a.varType, b.varType)
  }
}
object Builder {
  class Mapping[A <: String, B <: String](t1: Type.Size[A], t2: Type.Size[B]) extends Function1[Var[Type.Category[A]], Var[Type.Category[B]]] {
    override def apply(a: Var[Type.Category[A]]): Var[Type.Category[B]] = new Var.Simple(a.id, new Type.Category(t2))
  }
  class Incomplete[Deps <: HList, E <: Type](val id: VarID, val varType: E, val observation: Option[Observation]) {
    import Type.{ Vec, Size, Category }
    def *[I <: String](dim: Var[Size[I]])(implicit ctx: Builder, ev: Deps =:= (I :: HNil)): Var[Vec[I, E]] = {
      val v = new Var.Simple(id, varType)
      ctx.registerVar(v, None)
      observation.foreach(ctx.setObservation(v.id, _))
      v * dim
    }

    def *[I1 <: String, I2 <: String](dim1: Var[Size[I1]], dim2: Var[Vec[I1, Size[I2]]])(implicit ctx: Builder, ev: Deps =:= (I1 :: HNil)): Var[Vec[I1, Vec[I2, E]]] = {
      val v = new Var.Simple(id, varType)
      ctx.registerVar(v, None)
      observation.foreach(ctx.setObservation(v.id, _))
      v * (dim1, dim2)
    }
  }

  class VarDef[ID <: String](
      idStr: String,
      ctx: Builder,
      observation: Option[Observation],
      desc: Option[String]
  ) {
    private[this] val id = new VarID(idStr)
    private[this] def register[T <: Type](v: Var[T]): Var[T] = {
      ctx.registerVar(v, desc)
      observation match {
        case Some(Observation.Given) =>
          ctx.setGenerator(v.id, Generator.Given())
        case Some(Observation.Observed) =>
          ctx.setGenerator(v.id, Generator.Given())
        case Some(_) | None =>
      }
      observation.foreach { o =>
        ctx.setObservation(v.id, o)
      }
      v
    }

    def size: Var[Type.Size[ID]] =
      register(new Var.Simple(id, Type.Size(id.asIndex)))

    def vec[I <: String, T <: Type](dim: Var[Type.Size[I]], tpe: T): Var[Type.Vec[I, T]] =
      register(new Var.Simple(id, Type.Vec(dim.id.asIndex, tpe)))

    def vec[I <: String, II <: HList, T <: Type](dim: Incomplete[II, Type.Size[I]], tpe: T): Incomplete[II, Type.Vec[I, T]] =
      new Incomplete(id, Type.Vec(dim.id.asIndex, tpe), observation)

    def realVec[I <: String, T <: Type](dim: Var[Type.Size[I]]): Var[Type.Vec[I, Type.Real]] =
      vec(dim, Type.Real)

    def realVec[I <: String, II <: HList, T <: Type](dim: Incomplete[II, Type.Size[I]]): Incomplete[II, Type.Vec[I, Type.Real]] =
      vec(dim, Type.Real)

    def binaryVec[I <: String, T <: Type](dim: Var[Type.Size[I]]): Var[Type.Vec[I, Type.Binary]] =
      vec(dim, Type.Binary)

    def binaryVec[I <: String, II <: HList, T <: Type](dim: Incomplete[II, Type.Size[I]]): Incomplete[II, Type.Vec[I, Type.Binary]] =
      vec(dim, Type.Binary)

    def category[I <: String](size: Var[Type.Size[I]]): Var[Type.Category[I]] =
      register(new Var.Simple(id, Type.Category(size.varType)))

    def category[I <: String, II <: HList](size: Incomplete[II, Type.Size[I]]): Incomplete[II, Type.Category[I]] =
      new Incomplete(id, Type.Category(size.varType), observation)

    def R: Var[Type.Real] =
      register(new Var.Simple(id, Type.Real))

    def real: Var[Type.Real] = R

    def binary: Var[Type.Binary] =
      register(new Var.Simple(id, Type.Binary))
  }

}
