package com.todesking.platebuilder

class Builder(id: String) { self =>
  import scala.collection.mutable
  import Builder.VarDef

  private[this] var vars: Map[VarID, (Type, String, Boolean)] = Map()
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

  def registerVar(v: Var[_ <: Type], repr: Option[String], observed: Boolean): Unit = {
    vars += (v.id -> (v.varType, v.id.str, observed))
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
      new VarDef[id.type](id, self, Some(Generator.Given(None)), opt(repr), true)

    def hidden(id: String, repr: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, Some(new Generator.Given(None)), opt(repr))

    def computed(id: String, repr: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, None, opt(repr))

    def dirichlet[I <: String](param: Var[Type.Vec[I, Type.Real]]): Generator.Sampled[Type.Vec[I, Type.Real]] =
      new Generator.Sampled(Some(s"Dirichlet(${param.id.str})"), param.deps + param.id)

    def multinominal[I <: String](param: Var[Type.Vec[I, Type.Real]]): Generator.Sampled[Type.Category[I]] =
      new Generator.Sampled(Some(s"Mult(${param.id.str})"), param.deps + param.id)
  }
}
object Builder {
  class Incomplete[Deps <: HList, E <: Type](val id: VarID, val varType: E, val observed: Boolean) {
    import Type.{ Vec, Size, Category }
    def *[I <: String](dim: Var[Size[I]])(implicit ctx: Builder, ev: Deps =:= (I :: HNil)): Var[Vec[I, E]] = {
      val v = new Var.Simple(id, varType)
      ctx.registerVar(v, None, observed)
      v * dim
    }

    def *[I1 <: String, I2 <: String](dim1: Var[Size[I1]], dim2: Var[Vec[I1, Size[I2]]])(implicit ctx: Builder, ev: Deps =:= (I1 :: HNil)): Var[Vec[I1, Vec[I2, E]]] = {
      val v = new Var.Simple(id, varType)
      ctx.registerVar(v, None, observed)
      v * (dim1, dim2)
    }
  }

  class VarDef[ID <: String](
      idStr: String,
      ctx: Builder, generator: Option[Generator[_ <: Type]],
      repr: Option[String],
      observed: Boolean = false
  ) {
    private[this] val id = new VarID(idStr)
    private[this] def register[T <: Type](v: Var[T]): Var[T] = {
      ctx.registerVar(v, repr, observed)
      generator.foreach { g =>
        ctx.setGenerator(v.id, g)
      }
      v
    }

    def size: Var[Type.Size[ID]] =
      register(new Var.Simple(id, Type.Size(id.asIndex)))

    def vec[I <: String, T <: Type](dim: Var[Type.Size[I]], tpe: T): Var[Type.Vec[I, T]] =
      register(new Var.Simple(id, Type.Vec(dim.id.asIndex, tpe)))

    def vec[I <: String, II <: HList, T <: Type](dim: Incomplete[II, Type.Size[I]], tpe: T): Incomplete[II, Type.Vec[I, T]] =
      new Incomplete(id, Type.Vec(dim.id.asIndex, tpe), observed)

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
      new Incomplete(id, Type.Category(size.varType), observed)

    def R: Var[Type.Real] =
      register(new Var.Simple(id, Type.Real))

    def B: Var[Type.Binary] =
      register(new Var.Simple(id, Type.Binary))
  }

}
