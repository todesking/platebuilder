package com.todesking.platebuilder

import scala.language.higherKinds

class DSL(val self: Builder) {
  import DSL.{ Namer, VarDef }
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

  def size: Namer.Size =
    new Namer.Size(self)

  def given(id: String, desc: String = ""): VarDef[id.type] =
    new VarDef[id.type](id, self, Some(Observation.Given), opt(desc))

  def given: Namer.VarDef =
    new Namer.VarDef(self, Some(Observation.Given))

  def observed(id: String, desc: String = ""): VarDef[id.type] =
    new VarDef[id.type](id, self, Some(Observation.Observed), opt(desc))

  def observed: Namer.VarDef =
    new Namer.VarDef(self, Some(Observation.Observed))

  def hidden(id: String, desc: String = ""): VarDef[id.type] =
    new VarDef[id.type](id, self, Some(Observation.Hidden), opt(desc))

  def hidden: Namer.VarDef =
    new Namer.VarDef(self, Some(Observation.Hidden))

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

object DSL {
  abstract class Namer[A[_ <: String]] {
    import Namer.{ literal => lit }

    protected def vdef[ID <: String](id: ID): A[ID]

    protected def vdef[ID <: String](id: ID, desc: String): A[ID]

    def Alpha: A[lit.Alpha.type] = vdef(lit.Alpha)
    def Alpha(desc: String): A[lit.Alpha.type] = vdef(lit.Alpha)
    def alpha: A[lit.alpha.type] = vdef(lit.alpha)
    def alpha(desc: String): A[lit.alpha.type] = vdef(lit.alpha)
    def Beta: A[lit.Beta.type] = vdef(lit.Beta)
    def Beta(desc: String): A[lit.Beta.type] = vdef(lit.Beta)
    def beta: A[lit.beta.type] = vdef(lit.beta)
    def beta(desc: String): A[lit.beta.type] = vdef(lit.beta)
    def Gamma: A[lit.Gamma.type] = vdef(lit.Gamma)
    def Gamma(desc: String): A[lit.Gamma.type] = vdef(lit.Gamma)
    def gamma: A[lit.gamma.type] = vdef(lit.gamma)
    def gamma(desc: String): A[lit.gamma.type] = vdef(lit.gamma)
    def Delta: A[lit.Delta.type] = vdef(lit.Delta)
    def Delta(desc: String): A[lit.Delta.type] = vdef(lit.Delta)
    def delta: A[lit.delta.type] = vdef(lit.delta)
    def delta(desc: String): A[lit.delta.type] = vdef(lit.delta)
    def Epsilon: A[lit.Epsilon.type] = vdef(lit.Epsilon)
    def Epsilon(desc: String): A[lit.Epsilon.type] = vdef(lit.Epsilon)
    def epsilon: A[lit.epsilon.type] = vdef(lit.epsilon)
    def epsilon(desc: String): A[lit.epsilon.type] = vdef(lit.epsilon)
    def Zeta: A[lit.Zeta.type] = vdef(lit.Zeta)
    def Zeta(desc: String): A[lit.Zeta.type] = vdef(lit.Zeta)
    def zeta: A[lit.zeta.type] = vdef(lit.zeta)
    def zeta(desc: String): A[lit.zeta.type] = vdef(lit.zeta)
    def Eta: A[lit.Eta.type] = vdef(lit.Eta)
    def Eta(desc: String): A[lit.Eta.type] = vdef(lit.Eta)
    def eta: A[lit.eta.type] = vdef(lit.eta)
    def eta(desc: String): A[lit.eta.type] = vdef(lit.eta)
    def Theta: A[lit.Theta.type] = vdef(lit.Theta)
    def Theta(desc: String): A[lit.Theta.type] = vdef(lit.Theta)
    def theta: A[lit.theta.type] = vdef(lit.theta)
    def theta(desc: String): A[lit.theta.type] = vdef(lit.theta)
    def Iota: A[lit.Iota.type] = vdef(lit.Iota)
    def Iota(desc: String): A[lit.Iota.type] = vdef(lit.Iota)
    def iota: A[lit.iota.type] = vdef(lit.iota)
    def iota(desc: String): A[lit.iota.type] = vdef(lit.iota)
    def Kappa: A[lit.Kappa.type] = vdef(lit.Kappa)
    def Kappa(desc: String): A[lit.Kappa.type] = vdef(lit.Kappa)
    def kappa: A[lit.kappa.type] = vdef(lit.kappa)
    def kappa(desc: String): A[lit.kappa.type] = vdef(lit.kappa)
    def Lambda: A[lit.Lambda.type] = vdef(lit.Lambda)
    def Lambda(desc: String): A[lit.Lambda.type] = vdef(lit.Lambda)
    def lambda: A[lit.lambda.type] = vdef(lit.lambda)
    def lambda(desc: String): A[lit.lambda.type] = vdef(lit.lambda)
    def Mu: A[lit.Mu.type] = vdef(lit.Mu)
    def Mu(desc: String): A[lit.Mu.type] = vdef(lit.Mu)
    def mu: A[lit.mu.type] = vdef(lit.mu)
    def mu(desc: String): A[lit.mu.type] = vdef(lit.mu)
    def Nu: A[lit.Nu.type] = vdef(lit.Nu)
    def Nu(desc: String): A[lit.Nu.type] = vdef(lit.Nu)
    def nu: A[lit.nu.type] = vdef(lit.nu)
    def nu(desc: String): A[lit.nu.type] = vdef(lit.nu)
    def Xi: A[lit.Xi.type] = vdef(lit.Xi)
    def Xi(desc: String): A[lit.Xi.type] = vdef(lit.Xi)
    def xi: A[lit.xi.type] = vdef(lit.xi)
    def xi(desc: String): A[lit.xi.type] = vdef(lit.xi)
    def Omicron: A[lit.Omicron.type] = vdef(lit.Omicron)
    def Omicron(desc: String): A[lit.Omicron.type] = vdef(lit.Omicron)
    def omicron: A[lit.omicron.type] = vdef(lit.omicron)
    def omicron(desc: String): A[lit.omicron.type] = vdef(lit.omicron)
    def Pi: A[lit.Pi.type] = vdef(lit.Pi)
    def Pi(desc: String): A[lit.Pi.type] = vdef(lit.Pi)
    def pi: A[lit.pi.type] = vdef(lit.pi)
    def pi(desc: String): A[lit.pi.type] = vdef(lit.pi)
    def Rho: A[lit.Rho.type] = vdef(lit.Rho)
    def Rho(desc: String): A[lit.Rho.type] = vdef(lit.Rho)
    def rho: A[lit.rho.type] = vdef(lit.rho)
    def rho(desc: String): A[lit.rho.type] = vdef(lit.rho)
    def Sigma: A[lit.Sigma.type] = vdef(lit.Sigma)
    def Sigma(desc: String): A[lit.Sigma.type] = vdef(lit.Sigma)
    def sigma: A[lit.sigma.type] = vdef(lit.sigma)
    def sigma(desc: String): A[lit.sigma.type] = vdef(lit.sigma)
    def Tau: A[lit.Tau.type] = vdef(lit.Tau)
    def Tau(desc: String): A[lit.Tau.type] = vdef(lit.Tau)
    def tau: A[lit.tau.type] = vdef(lit.tau)
    def tau(desc: String): A[lit.tau.type] = vdef(lit.tau)
    def Upsilon: A[lit.Upsilon.type] = vdef(lit.Upsilon)
    def Upsilon(desc: String): A[lit.Upsilon.type] = vdef(lit.Upsilon)
    def upsilon: A[lit.upsilon.type] = vdef(lit.upsilon)
    def upsilon(desc: String): A[lit.upsilon.type] = vdef(lit.upsilon)
    def Phi: A[lit.Phi.type] = vdef(lit.Phi)
    def Phi(desc: String): A[lit.Phi.type] = vdef(lit.Phi)
    def phi: A[lit.phi.type] = vdef(lit.phi)
    def phi(desc: String): A[lit.phi.type] = vdef(lit.phi)
    def Chi: A[lit.Chi.type] = vdef(lit.Chi)
    def Chi(desc: String): A[lit.Chi.type] = vdef(lit.Chi)
    def chi: A[lit.chi.type] = vdef(lit.chi)
    def chi(desc: String): A[lit.chi.type] = vdef(lit.chi)
    def Psi: A[lit.Psi.type] = vdef(lit.Psi)
    def Psi(desc: String): A[lit.Psi.type] = vdef(lit.Psi)
    def psi: A[lit.psi.type] = vdef(lit.psi)
    def psi(desc: String): A[lit.psi.type] = vdef(lit.psi)
    def Omega: A[lit.Omega.type] = vdef(lit.Omega)
    def Omega(desc: String): A[lit.Omega.type] = vdef(lit.Omega)
    def omega: A[lit.omega.type] = vdef(lit.omega)
    def omega(desc: String): A[lit.omega.type] = vdef(lit.omega)

    def a: A[lit.a.type] = vdef(lit.a)
    def a(desc: String): A[lit.a.type] = vdef(lit.a, desc)
    def A: A[lit.A.type] = vdef(lit.A)
    def A(desc: String): A[lit.A.type] = vdef(lit.A, desc)
    def b: A[lit.b.type] = vdef(lit.b)
    def b(desc: String): A[lit.b.type] = vdef(lit.b, desc)
    def B: A[lit.B.type] = vdef(lit.B)
    def B(desc: String): A[lit.B.type] = vdef(lit.B, desc)
    def c: A[lit.c.type] = vdef(lit.c)
    def c(desc: String): A[lit.c.type] = vdef(lit.c, desc)
    def C: A[lit.C.type] = vdef(lit.C)
    def C(desc: String): A[lit.C.type] = vdef(lit.C, desc)
    def d: A[lit.d.type] = vdef(lit.d)
    def d(desc: String): A[lit.d.type] = vdef(lit.d, desc)
    def D: A[lit.D.type] = vdef(lit.D)
    def D(desc: String): A[lit.D.type] = vdef(lit.D, desc)
    def e: A[lit.e.type] = vdef(lit.e)
    def e(desc: String): A[lit.e.type] = vdef(lit.e, desc)
    def E: A[lit.E.type] = vdef(lit.E)
    def E(desc: String): A[lit.E.type] = vdef(lit.E, desc)
    def f: A[lit.f.type] = vdef(lit.f)
    def f(desc: String): A[lit.f.type] = vdef(lit.f, desc)
    def F: A[lit.F.type] = vdef(lit.F)
    def F(desc: String): A[lit.F.type] = vdef(lit.F, desc)
    def g: A[lit.g.type] = vdef(lit.g)
    def g(desc: String): A[lit.g.type] = vdef(lit.g, desc)
    def G: A[lit.G.type] = vdef(lit.G)
    def G(desc: String): A[lit.G.type] = vdef(lit.G, desc)
    def h: A[lit.h.type] = vdef(lit.h)
    def h(desc: String): A[lit.h.type] = vdef(lit.h, desc)
    def H: A[lit.H.type] = vdef(lit.H)
    def H(desc: String): A[lit.H.type] = vdef(lit.H, desc)
    def i: A[lit.i.type] = vdef(lit.i)
    def i(desc: String): A[lit.i.type] = vdef(lit.i, desc)
    def I: A[lit.I.type] = vdef(lit.I)
    def I(desc: String): A[lit.I.type] = vdef(lit.I, desc)
    def j: A[lit.j.type] = vdef(lit.j)
    def j(desc: String): A[lit.j.type] = vdef(lit.j, desc)
    def J: A[lit.J.type] = vdef(lit.J)
    def J(desc: String): A[lit.J.type] = vdef(lit.J, desc)
    def k: A[lit.k.type] = vdef(lit.k)
    def k(desc: String): A[lit.k.type] = vdef(lit.k, desc)
    def K: A[lit.K.type] = vdef(lit.K)
    def K(desc: String): A[lit.K.type] = vdef(lit.K, desc)
    def l: A[lit.l.type] = vdef(lit.l)
    def l(desc: String): A[lit.l.type] = vdef(lit.l, desc)
    def L: A[lit.L.type] = vdef(lit.L)
    def L(desc: String): A[lit.L.type] = vdef(lit.L, desc)
    def m: A[lit.m.type] = vdef(lit.m)
    def m(desc: String): A[lit.m.type] = vdef(lit.m, desc)
    def M: A[lit.M.type] = vdef(lit.M)
    def M(desc: String): A[lit.M.type] = vdef(lit.M, desc)
    def n: A[lit.n.type] = vdef(lit.n)
    def n(desc: String): A[lit.n.type] = vdef(lit.n, desc)
    def N: A[lit.N.type] = vdef(lit.N)
    def N(desc: String): A[lit.N.type] = vdef(lit.N, desc)
    def o: A[lit.o.type] = vdef(lit.o)
    def o(desc: String): A[lit.o.type] = vdef(lit.o, desc)
    def O: A[lit.O.type] = vdef(lit.O)
    def O(desc: String): A[lit.O.type] = vdef(lit.O, desc)
    def p: A[lit.p.type] = vdef(lit.p)
    def p(desc: String): A[lit.p.type] = vdef(lit.p, desc)
    def P: A[lit.P.type] = vdef(lit.P)
    def P(desc: String): A[lit.P.type] = vdef(lit.P, desc)
    def q: A[lit.q.type] = vdef(lit.q)
    def q(desc: String): A[lit.q.type] = vdef(lit.q, desc)
    def Q: A[lit.Q.type] = vdef(lit.Q)
    def Q(desc: String): A[lit.Q.type] = vdef(lit.Q, desc)
    def r: A[lit.r.type] = vdef(lit.r)
    def r(desc: String): A[lit.r.type] = vdef(lit.r, desc)
    def R: A[lit.R.type] = vdef(lit.R)
    def R(desc: String): A[lit.R.type] = vdef(lit.R, desc)
    def s: A[lit.s.type] = vdef(lit.s)
    def s(desc: String): A[lit.s.type] = vdef(lit.s, desc)
    def S: A[lit.S.type] = vdef(lit.S)
    def S(desc: String): A[lit.S.type] = vdef(lit.S, desc)
    def t: A[lit.t.type] = vdef(lit.t)
    def t(desc: String): A[lit.t.type] = vdef(lit.t, desc)
    def T: A[lit.T.type] = vdef(lit.T)
    def T(desc: String): A[lit.T.type] = vdef(lit.T, desc)
    def u: A[lit.u.type] = vdef(lit.u)
    def u(desc: String): A[lit.u.type] = vdef(lit.u, desc)
    def U: A[lit.U.type] = vdef(lit.U)
    def U(desc: String): A[lit.U.type] = vdef(lit.U, desc)
    def v: A[lit.v.type] = vdef(lit.v)
    def v(desc: String): A[lit.v.type] = vdef(lit.v, desc)
    def V: A[lit.V.type] = vdef(lit.V)
    def V(desc: String): A[lit.V.type] = vdef(lit.V, desc)
    def w: A[lit.w.type] = vdef(lit.w)
    def w(desc: String): A[lit.w.type] = vdef(lit.w, desc)
    def W: A[lit.W.type] = vdef(lit.W)
    def W(desc: String): A[lit.W.type] = vdef(lit.W, desc)
    def x: A[lit.x.type] = vdef(lit.x)
    def x(desc: String): A[lit.x.type] = vdef(lit.x, desc)
    def X: A[lit.X.type] = vdef(lit.X)
    def X(desc: String): A[lit.X.type] = vdef(lit.X, desc)
    def y: A[lit.y.type] = vdef(lit.y)
    def y(desc: String): A[lit.y.type] = vdef(lit.y, desc)
    def Y: A[lit.Y.type] = vdef(lit.Y)
    def Y(desc: String): A[lit.Y.type] = vdef(lit.Y, desc)
    def z: A[lit.z.type] = vdef(lit.z)
    def z(desc: String): A[lit.z.type] = vdef(lit.z, desc)
    def Z: A[lit.Z.type] = vdef(lit.Z)
    def Z(desc: String): A[lit.Z.type] = vdef(lit.Z, desc)
  }
  object Namer {
    class VarDef(builder: Builder, observation: Option[Observation]) extends Namer[DSL.VarDef] {
      override protected def vdef[ID <: String](id: ID): DSL.VarDef[ID] =
        new DSL.VarDef(id, builder, observation, None)

      override protected def vdef[ID <: String](id: ID, desc: String): DSL.VarDef[ID] =
        new DSL.VarDef(id, builder, observation, Some(desc))
    }
    class Size(builder: Builder) extends Namer[({ type L[A <: String] = Var[Type.Size[A]] })#L] {
      override protected def vdef[ID <: String](id: ID): Var[Type.Size[ID]] =
        new DSL.VarDef(id, builder, Some(Observation.Given), None).size

      override protected def vdef[ID <: String](id: ID, desc: String): Var[Type.Size[ID]] =
        new DSL.VarDef(id, builder, Some(Observation.Given), Some(desc)).size
    }
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
      final val a = "a"
      final val A = "A"
      final val b = "b"
      final val B = "B"
      final val c = "c"
      final val C = "C"
      final val d = "d"
      final val D = "D"
      final val e = "e"
      final val E = "E"
      final val f = "f"
      final val F = "F"
      final val g = "g"
      final val G = "G"
      final val h = "h"
      final val H = "H"
      final val i = "i"
      final val I = "I"
      final val j = "j"
      final val J = "J"
      final val k = "k"
      final val K = "K"
      final val l = "l"
      final val L = "L"
      final val m = "m"
      final val M = "M"
      final val n = "n"
      final val N = "N"
      final val o = "o"
      final val O = "O"
      final val p = "p"
      final val P = "P"
      final val q = "q"
      final val Q = "Q"
      final val r = "r"
      final val R = "R"
      final val s = "s"
      final val S = "S"
      final val t = "t"
      final val T = "T"
      final val u = "u"
      final val U = "U"
      final val v = "v"
      final val V = "V"
      final val w = "w"
      final val W = "W"
      final val x = "x"
      final val X = "X"
      final val y = "y"
      final val Y = "Y"
      final val z = "z"
      final val Z = "Z"
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
}
