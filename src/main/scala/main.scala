package com.todesking.platebuilder

import scala.language.implicitConversions
import scala.language.higherKinds

object Main {
  val Unigram = Model.define("Unigram") { implicit ctx =>
    import ctx.dsl._
    val V = size("V", "Number of vocabularies")
    val D = size("D", "Number of documents")
    val N = size("N", "Number of words for each documents") * D

    // hyperparameters
    val beta = given("β").realVec(V)

    // variables
    val phi = hidden("φ").realVec(V)
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
    val K = size("K", "Number of topics")
    val V = size("V", "Number of vocabularies")
    val D = size("D", "Number of documents")
    val N = size("N", "Number of words for each documents") * D

    // hyperparameters
    val alpha = given("α").realVec(K)
    val beta = given("β").realVec(V)

    // variables
    val phi = hidden("φ").realVec(V) * K
    val theta = hidden("θ").realVec(K)
    val z = hidden("z", "Hidden topic for each document").category(K) * D
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
    val K = size("K", "Number of topics")
    val V = size("V", "Number of vocabularies")
    val D = size("D", "Number of documents")
    val N = size("N", "Number of words for each documents") * D

    // hyperparameters
    val alpha = given("α").realVec(K)
    val beta = given("β").realVec(V)

    // variables
    val phi = hidden("φ").realVec(V) * K
    val theta = hidden("θ").realVec(K) * D
    val z = hidden("z", "Hidden topic for each word").category(K) * (D, N)
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
    val K = size("K", "Num of topics")
    val V = size("V", "Num of vocabularies")
    val D = size("D", "Num of documents")
    val N = size("N", "Num of words for each document") * D
    val L = size("L", "Num of labels")
    val Kd = computed("Kd", "Num of labels assigned for each document").size * D
    val K_L = size("K_L", "Num of topics assigned for each document and label") * (D, Kd)

    // hyperparameters
    val alpha = given("α").R
    val beta = given("β").realVec(V)

    val alphaL = computed("α_L").realVec(Kd(D)) * D
    val alphaT = computed("α_T").realVec(K) * (D, Kd)

    // variables
    val phi = hidden("φ").realVec(V) * K
    val theta = hidden("θ", "Topic distribution").realVec(K) * (D, Kd)
    val Lambda = observed("Λ", "Assigned labels").binaryVec(L) * D
    val psi = hidden("ψ").realVec(Kd(D)) * D
    val z = hidden("z").category(K) * (D, N)
    val w = observed("w").category(V) * (D, N)
    val l = hidden("l").category(Kd(D)) * (D, N)

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    for (d <- D) {
      Kd(d) ~ deterministic"|${Lambda(d)}|"
      implicit val kd2l = mapping(Kd(d), L)

      for (l <- Kd(d)) {
        for (k <- K) {
          alphaT(d, l, k) ~ deterministic"$alpha * ${K_L(d, l)} if topic ${k} is assigned to ${Lambda(d, l)}\n0 otherwise"
        }
        alphaL(d, l) ~ deterministic"$alpha * ${K_L(d, l)} * ${Lambda(d, kd2l(l))}"
        theta(d, l) ~ dirichlet(alphaT(d, l))
      }

      psi(d) ~ dirichlet(alphaL(d))

      for (n <- N(d)) {
        l(d, n) ~ multinominal(psi(d))
        z(d, n) ~ multinominal(theta(d, l(d, n)))
        w(d, n) ~ multinominal(phi(z(d, n)))
      }
    }
  }

  val BLR = Model.define("BayesianLinearRegression") { implicit ctx =>
    import ctx.dsl._

    val N = size("N", "Num of data points")
    val D = size("D", "Num of features")

    val s2_w = given("σ^2_w").R
    val s2_b = given("σ^2_b").R
    val s2_y = given("σ^2_y").R

    val x = observed("x").realVec(D) * N
    val w = hidden("w").realVec(D)
    val b = hidden("b").R
    val mu = hidden("μ").R * N
    val y = observed("y").R * N

    for (d <- D) {
      w(d) ~ normal(const(0.0), s2_w)
    }

    b ~ normal(const(0.0), s2_b)

    for (n <- N) {
      mu(n) ~ stochastic"${x(n)} * $w + $b"
      y(n) ~ normal(mu(n), s2_y)
    }

  }

  val Sample = Model.define("Sample") { implicit ctx =>
    import ctx.dsl._
    val D = size("D")
    val C = size("C") * D
    val S = size("S") * (D, C)
    val P = size("P") * (D, C, S)
    val N = size("N") * (D, C, S, P)
    val V = size("V")

    val K = size("K")

    val unused = given("unused").realVec(D) * D

    val alpha = given("α").realVec(K)
    val beta = given("β").realVec(V)

    val phi = hidden("φ").realVec(V) * K
    val eta = hidden("η").realVec(K) * D
    val zeta = hidden("ζ").category(K) * K * D
    val theta = hidden("θ").realVec(K) * (D, C)
    val z = hidden("z").category(K) * (D, C, S, P, N)
    val w = observed("w").category(V) * (D, C, S, P, N)
    val y = observed("y").category(K) * (D, C, S)
    val a = observed("a").R * (D, C)

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    for (d <- D) {
      eta(d) ~ dirichlet(alpha)
      for (k <- K) {
        zeta(d, k) ~ multinominal(eta(d))
      }
      for (c <- C(d)) {
        theta(d, c) ~ dirichlet(eta(d))
        for (s <- S(d, c)) {
          y(d, c, s) ~ deterministic"f(${z(d, c, s)})"
          for (p <- P(d, c, s)) {
            for (n <- N(d, c, s, p)) {
              z(d, c, s, p, n) ~ multinominal(theta(d, c))
              w(d, c, s, p, n) ~ multinominal(phi(zeta(d, z(d, c, s, p, n))))
            }
          }
        }
        a(d, c) ~ stochastic"SomeDist(${w(d, c)})"
      }
    }
  }

  val Legend = Model.define("Legend") { implicit ctx =>
    import ctx.dsl._
    val Size = size("Size")
    val hiddenVar = hidden("H", "Hidden variable").R
    val observedVar = observed("O", "Observed variable").R
    val givenVar = given("G", "Given constant(Hyper parameter)").R
    val det = hidden("D", "Deterministic computation").R * Size
    val sto = hidden("S", "Stochastic computation").R * Size

    hiddenVar ~ normal(observedVar, const(1.0))

    for (s <- Size) {
      det(s) ~ deterministic"f(${givenVar})"
      sto(s) ~ stochastic"g(${givenVar})"
    }
  }

  val models = Seq(Unigram, MixtureOfUnigrams, LDA, PLDA, BLR, Sample, Legend)

  def main(args: Array[String]): Unit = {
    println(Model.toDot(models))
  }
}
