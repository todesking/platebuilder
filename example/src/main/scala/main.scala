package com.todesking.platebuilder

import scala.language.implicitConversions
import scala.language.higherKinds

object Main {

  val BLR = Model.define("BayesianLinearRegression") { implicit builder =>
    import builder.dsl._

    val N = size.N("Num of data points")
    val D = size.D("Num of features")

    val s2_w = given("σ^2_w").R
    val s2_b = given("σ^2_b").R
    val s2_y = given("σ^2_y").R

    val x = observed.x.realVec(D) * N
    val w = hidden.w.realVec(D)
    val b = hidden.b.R
    val mu = hidden.mu.R * N
    val y = observed.y.R * N

    for (d <- D) {
      w(d) ~ normal(const(0.0), s2_w)
    }

    b ~ normal(const(0.0), s2_b)

    for (n <- N) {
      mu(n) ~ stochastic"${x(n)} * $w + $b"
      y(n) ~ normal(mu(n), s2_y)
    }

  }

  val Sample = Model.define("Sample") { implicit builder =>
    import builder.dsl._
    val D = size.D
    val C = size.C * D
    val S = size.S * (D, C)
    val P = size.P * (D, C, S)
    val N = size.N * (D, C, S, P)
    val V = size.V

    val K = size.K

    val unused = given("unused").realVec(D) * D

    val alpha = given.alpha.realVec(K)
    val beta = given.beta.realVec(V)

    val phi = hidden.phi.realVec(V) * K
    val eta = hidden.eta.realVec(K) * D
    val zeta = hidden.zeta.category(K) * K * D
    val theta = hidden.theta.realVec(K) * (D, C)
    val z = hidden.z.category(K) * (D, C, S, P, N)
    val w = observed.w.category(V) * (D, C, S, P, N)
    val y = observed.y.category(K) * (D, C, S)
    val a = observed.a.R * (D, C)
    val b = hidden.b.realVec(S(D)(C(D))) * (D, C)

    for (d <- D) {
      for (c <- C(d)) {
        for (s <- S(d, c)) {
          b(d, c, s) ~ deterministic"${const(0.0)}"
        }
      }
    }

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    for (d <- D) {
      eta(d) ~ dirichlet(alpha)
      for (k <- K) {
        zeta(d, k) ~ categorical(eta(d))
      }
      for (c <- C(d)) {
        theta(d, c) ~ dirichlet(eta(d))
        for (s <- S(d, c)) {
          y(d, c, s) ~ deterministic"f(${z(d, c, s)})"
          for (p <- P(d, c, s)) {
            for (n <- N(d, c, s, p)) {
              z(d, c, s, p, n) ~ categorical(theta(d, c))
              w(d, c, s, p, n) ~ categorical(phi(zeta(d, z(d, c, s, p, n))))
            }
          }
        }
        a(d, c) ~ stochastic"SomeDist(${w(d, c)})"
      }
    }
  }

  val Legend = Model.define("Legend") { implicit builder =>
    import builder.dsl._
    val Size = size("Size")
    val hiddenVar = hidden.H("Hidden variable").R
    val observedVar = observed("Observed variable").R
    val givenVar = given.G("Given constant(Hyper parameter)").R
    val det = hidden.D("Deterministic computation").R * Size
    val sto = hidden.S("Stochastic computation").R * Size

    hiddenVar ~ normal(observedVar, const(1.0))

    for (s <- Size) {
      det(s) ~ deterministic"f(${givenVar})"
      sto(s) ~ stochastic"g(${givenVar})"
    }
  }

  val models = Seq(BLR, Sample, Legend)

  def main(args: Array[String]): Unit = {
    println(Model.toDot(models))
  }
}
