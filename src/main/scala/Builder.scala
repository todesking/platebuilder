package com.todesking.platebuilder

class Builder(id: String) { self =>
  import scala.collection.mutable
  import Builder.{ VarDef, Namer }

  private[this] var vars: Map[VarID, Var[_ <: Type]] = Map()
  private[this] var varOrder: Seq[VarID] = Seq()
  private[this] var indices: Map[VarID, Seq[IndexID]] = Map()
  private[this] val inEdges: mutable.MultiMap[VarID, VarID] =
    new mutable.HashMap[VarID, mutable.Set[VarID]] with mutable.MultiMap[VarID, VarID]
  private[this] var generators: Map[VarID, Generator[_ <: Type]] = Map()
  private[this] var descs: Map[VarID, String] = Map()
  private[this] var observations: Map[VarID, Observation] = Map()

  private[this] var _currentPath: Seq[IndexID] = Seq()

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

  def newVar(v: Var[_ <: Type], observation: Option[Observation], description: Option[String]): Unit = {
    setVar(v)
    description.foreach(setDescription(v.id, _))
    observation match {
      case Some(Observation.Given) =>
        setGenerator(v, Seq(), Generator.Given(), false)
      case Some(Observation.Observed) =>
        setGenerator(v, Seq(), Generator.Given(), false)
      case Some(_) | None =>
    }
    observation.foreach(setObservation(v.id, _))
  }

  def newConst(v: Var[_ <: Type], repr: String): Unit = {
    setVar(v)
    setObservation(v.id, Observation.Given)
    setGenerator(v, Seq(), Generator.Const(repr), true)
  }

  private[this] def setVar(v: Var[_ <: Type]): Unit = {
    if (!vars.contains(v.id)) {
      varOrder :+= v.id
    }
    vars += (v.id -> v)
  }

  private[this] def setDescription(vid: VarID, desc: String): Unit = {
    descs += (vid -> desc)
  }

  private[this] def setObservation(v: VarID, o: Observation): Unit = {
    observations += (v -> o)
  }

  def addIndex(id: VarID, i: Seq[IndexID]): Unit = {
    indices.get(id).fold {
      indices += (id -> i)
    } { is =>
      indices += (id -> (i ++ is))
    }
  }

  def withIndex[A](i: IndexID)(f: => A): A = {
    _currentPath :+= i
    val ret = f
    _currentPath = _currentPath.take(_currentPath.size - 1)
    ret
  }

  def setGenerator(v: Var[_ <: Type], path: Seq[IndexID], g: Generator[_ <: Type], ignoreCurrentPath: Boolean): Unit = {
    setVar(v)
    inEdges.remove(v.id)
    if (!ignoreCurrentPath && path != _currentPath) {
      val p1 = _currentPath.map(_.str).mkString("(", ", ", ")")
      val p2 = path.map(_.str).mkString("(", ", ", ")")
      throw new RuntimeException(s"Path not match for ${v.id.str}: expected ${p1} but ${p2}")
    }
    indices += (v.id -> path)
    g.dependencies.foreach { d =>
      inEdges.addBinding(v.id, d)
    }
    generators += (v.id -> g)
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
      self.newConst(v, n.toString)
      v
    }

    def size(id: String, desc: String = ""): Var[Type.Size[id.type]] =
      given(id, desc).size

    def given(id: String, desc: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, Some(Observation.Given), opt(desc))

    def given: Namer =
      new Namer(self, Some(Observation.Given))

    def observed(id: String, desc: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, Some(Observation.Observed), opt(desc))

    def observed: Namer =
      new Namer(self, Some(Observation.Observed))

    def hidden(id: String, desc: String = ""): VarDef[id.type] =
      new VarDef[id.type](id, self, Some(Observation.Hidden), opt(desc))

    def hidden: Namer =
      new Namer(self, Some(Observation.Hidden))

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
    def *[I <: String](dim: Var[Size[I]])(implicit ctx: Builder, ev: HList.ContainsOnly[Deps, I]): Var[Vec[I, E]] = {
      val v = new Var.Simple(id, varType)
      ctx.newVar(v, observation, None)
      v * dim
    }

    def *[I1 <: String, I2 <: String](dim1: Var[Size[I1]], dim2: Var[Vec[I1, Size[I2]]])(implicit ctx: Builder, ev: HList.ContainsOnly[Deps, I1]): Var[Vec[I1, Vec[I2, E]]] = {
      val v = new Var.Simple(id, varType)
      ctx.newVar(v, observation, None)
      v * (dim1, dim2)
    }
  }
  object Incomplete {
    import Type.{ Size, Vec }
    implicit class Vec1Ops[Deps <: HList, E <: Type, I <: String](self: Incomplete[Deps, Vec[I, E]]) {
      def apply[D2 <: HList](i: Incomplete[D2, Size[I]])(implicit aux: Aux[Deps, D2, E, I]): aux.Result =
        aux(self, i)
    }

    sealed abstract class Aux[D1 <: HList, D2 <: HList, E <: Type, I <: String] {
      type Result
      def apply(self: Incomplete[D1, Vec[I, E]], i: Incomplete[D2, Size[I]]): Result
    }
    object Aux {
      sealed abstract class HasDep[D1 <: HList, D2 <: HList, D <: HList, E <: Type, I <: String] extends Aux[D1, D2, E, I] {
        override type Result = Incomplete[D, E]
      }

      implicit def incomplete[D1 <: HList, D2 <: HList, E <: Type, I <: String](
        implicit
        ev: HList.Concat[D1, D2]
      ): HasDep[D1, D2, ev.Result, E, I] =
        new HasDep[D1, D2, ev.Result, E, I] {
          override def apply(self: Incomplete[D1, Vec[I, E]], i: Incomplete[D2, Size[I]]) =
            new Incomplete(self.id, self.varType.elementType, self.observation)
        }
    }
  }

  class Namer(builder: Builder, observation: Option[Observation]) {
    import Namer.{ literal => lit }

    private[this] def vdef[ID <: String](id: ID): VarDef[ID] =
      new VarDef(id, builder, observation, None)

    private[this] def vdef[ID <: String](id: ID, desc: String): VarDef[ID] =
      new VarDef(id, builder, observation, Some(desc))

    def Alpha = vdef(lit.Alpha)
    def Alpha(desc: String) = vdef(lit.Alpha)
    def alpha = vdef(lit.alpha)
    def alpha(desc: String) = vdef(lit.alpha)
    def Beta = vdef(lit.Beta)
    def Beta(desc: String) = vdef(lit.Beta)
    def beta = vdef(lit.beta)
    def beta(desc: String) = vdef(lit.beta)
    def Gamma = vdef(lit.Gamma)
    def Gamma(desc: String) = vdef(lit.Gamma)
    def gamma = vdef(lit.gamma)
    def gamma(desc: String) = vdef(lit.gamma)
    def Delta = vdef(lit.Delta)
    def Delta(desc: String) = vdef(lit.Delta)
    def delta = vdef(lit.delta)
    def delta(desc: String) = vdef(lit.delta)
    def Epsilon = vdef(lit.Epsilon)
    def Epsilon(desc: String) = vdef(lit.Epsilon)
    def epsilon = vdef(lit.epsilon)
    def epsilon(desc: String) = vdef(lit.epsilon)
    def Zeta = vdef(lit.Zeta)
    def Zeta(desc: String) = vdef(lit.Zeta)
    def zeta = vdef(lit.zeta)
    def zeta(desc: String) = vdef(lit.zeta)
    def Eta = vdef(lit.Eta)
    def Eta(desc: String) = vdef(lit.Eta)
    def eta = vdef(lit.eta)
    def eta(desc: String) = vdef(lit.eta)
    def Theta = vdef(lit.Theta)
    def Theta(desc: String) = vdef(lit.Theta)
    def theta = vdef(lit.theta)
    def theta(desc: String) = vdef(lit.theta)
    def Iota = vdef(lit.Iota)
    def Iota(desc: String) = vdef(lit.Iota)
    def iota = vdef(lit.iota)
    def iota(desc: String) = vdef(lit.iota)
    def Kappa = vdef(lit.Kappa)
    def Kappa(desc: String) = vdef(lit.Kappa)
    def kappa = vdef(lit.kappa)
    def kappa(desc: String) = vdef(lit.kappa)
    def Lambda = vdef(lit.Lambda)
    def Lambda(desc: String) = vdef(lit.Lambda)
    def lambda = vdef(lit.lambda)
    def lambda(desc: String) = vdef(lit.lambda)
    def Mu = vdef(lit.Mu)
    def Mu(desc: String) = vdef(lit.Mu)
    def mu = vdef(lit.mu)
    def mu(desc: String) = vdef(lit.mu)
    def Nu = vdef(lit.Nu)
    def Nu(desc: String) = vdef(lit.Nu)
    def nu = vdef(lit.nu)
    def nu(desc: String) = vdef(lit.nu)
    def Xi = vdef(lit.Xi)
    def Xi(desc: String) = vdef(lit.Xi)
    def xi = vdef(lit.xi)
    def xi(desc: String) = vdef(lit.xi)
    def Omicron = vdef(lit.Omicron)
    def Omicron(desc: String) = vdef(lit.Omicron)
    def omicron = vdef(lit.omicron)
    def omicron(desc: String) = vdef(lit.omicron)
    def Pi = vdef(lit.Pi)
    def Pi(desc: String) = vdef(lit.Pi)
    def pi = vdef(lit.pi)
    def pi(desc: String) = vdef(lit.pi)
    def Rho = vdef(lit.Rho)
    def Rho(desc: String) = vdef(lit.Rho)
    def rho = vdef(lit.rho)
    def rho(desc: String) = vdef(lit.rho)
    def Sigma = vdef(lit.Sigma)
    def Sigma(desc: String) = vdef(lit.Sigma)
    def sigma = vdef(lit.sigma)
    def sigma(desc: String) = vdef(lit.sigma)
    def Tau = vdef(lit.Tau)
    def Tau(desc: String) = vdef(lit.Tau)
    def tau = vdef(lit.tau)
    def tau(desc: String) = vdef(lit.tau)
    def Upsilon = vdef(lit.Upsilon)
    def Upsilon(desc: String) = vdef(lit.Upsilon)
    def upsilon = vdef(lit.upsilon)
    def upsilon(desc: String) = vdef(lit.upsilon)
    def Phi = vdef(lit.Phi)
    def Phi(desc: String) = vdef(lit.Phi)
    def phi = vdef(lit.phi)
    def phi(desc: String) = vdef(lit.phi)
    def Chi = vdef(lit.Chi)
    def Chi(desc: String) = vdef(lit.Chi)
    def chi = vdef(lit.chi)
    def chi(desc: String) = vdef(lit.chi)
    def Psi = vdef(lit.Psi)
    def Psi(desc: String) = vdef(lit.Psi)
    def psi = vdef(lit.psi)
    def psi(desc: String) = vdef(lit.psi)
    def Omega = vdef(lit.Omega)
    def Omega(desc: String) = vdef(lit.Omega)
    def omega = vdef(lit.omega)
    def omega(desc: String) = vdef(lit.omega)
  }
  object Namer {
    object literal {
      final val Alpha = "Α"
      final val alpha = "α"
      final val Beta = "Β"
      final val beta = "β"
      final val Gamma = "Γ"
      final val gamma = "γ"
      final val Delta = "Δ"
      final val delta = "δ"
      final val Epsilon = "Ε"
      final val epsilon = "ε"
      final val Zeta = "Ζ"
      final val zeta = "ζ"
      final val Eta = "Η"
      final val eta = "η"
      final val Theta = "Θ"
      final val theta = "θ"
      final val Iota = "Ι"
      final val iota = "ι"
      final val Kappa = "Κ"
      final val kappa = "κ"
      final val Lambda = "Λ"
      final val lambda = "λ"
      final val Mu = "Μ"
      final val mu = "μ"
      final val Nu = "Ν"
      final val nu = "ν"
      final val Xi = "Ξ"
      final val xi = "ξ"
      final val Omicron = "Ο"
      final val omicron = "ο"
      final val Pi = "Π"
      final val pi = "π"
      final val Rho = "Ρ"
      final val rho = "ρ"
      final val Sigma = "Σ"
      final val sigma = "σ"
      final val Tau = "Τ"
      final val tau = "τ"
      final val Upsilon = "Υ"
      final val upsilon = "υ"
      final val Phi = "Φ"
      final val phi = "φ"
      final val Chi = "Χ"
      final val chi = "χ"
      final val Psi = "Ψ"
      final val psi = "ψ"
      final val Omega = "Ω"
      final val omega = "ω"
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
      ctx.newVar(v, observation, desc)
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
