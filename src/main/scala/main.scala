package com.todesking.platebuilder

import scala.language.implicitConversions
import scala.language.higherKinds

object Main {
  val Unigram = Model.define("Unigram") { implicit ctx =>
    import ctx.dsl._
    val V = size("V") // num of vocabularies
    val D = size("D") // num of documens
    val N = size("N") * D // N(d): num of words in document d

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
    val K = size("K") // num of topics
    val V = size("V") // num of vocabularies
    val D = size("D") // num of documens
    val N = size("N") * D // N(d): num of words in document d

    // hyperparameters
    val alpha = given("α").realVec(K)
    val beta = given("β").realVec(V)

    // variables
    val phi = hidden("φ").realVec(V) * K
    val theta = hidden("θ").realVec(K)
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
    val alpha = given("α").realVec(K)
    val beta = given("β").realVec(V)

    // variables
    val phi = hidden("φ").realVec(V) * K
    val theta = hidden("θ").realVec(K) * D
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
    val alpha = given("α").R
    val beta = given("β").realVec(V)

    val alphaL = computed("α_L").realVec(Kd(D)) * D
    val alphaT = computed("α_T").realVec(K) * D

    // variables
    val phi = hidden("φ").realVec(V) * K
    val theta = hidden("θ").realVec(K) * (D, Kd) // theta(d, l): topic distribution of document d and label l
    val Lambda = observed("Λ").binaryVec(L) * D // Set of labels in document d as L-dimensional binary vector
    val psi = hidden("ψ").realVec(Kd(D)) * D
    val z = hidden("z").category(K) * (D, N)
    val w = observed("w").category(V) * (D, N)
    val l = hidden("l").category(Kd(D)) * (D, N)

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    for (d <- D) {
      Kd(d) ~ compute("|Λ_d|", Lambda(d))
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

  val Deep = Model.define("Deep") { implicit ctx =>
    import ctx.dsl._
    val D = size("D")
    val C = size("C") * D
    val S = size("S") * (D, C)
    val P = size("P") * (D, C, S)
    val N = size("N") * (D, C, S, P)
    val V = size("V")

    val K = size("K")

    val alpha = given("α").realVec(K)
    val beta = given("β").realVec(V)

    val phi = hidden("φ").realVec(V) * K
    val eta = hidden("η").realVec(K) * D
    val zeta = hidden("ζ").category(K) * K * D
    val theta = hidden("θ").realVec(K) * (D, C)
    val z = hidden("z").category(K) * (D, C, S, P, N)
    val w = observed("w").category(V) * (D, C, S, P, N)
    val y = observed("y").category(K) * (D, C, S)

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
          y(d, c, s) ~ compute(z(d, c, s))
          for (p <- P(d, c, s)) {
            for (n <- N(d, c, s, p)) {
              z(d, c, s, p, n) ~ multinominal(theta(d, c))
              w(d, c, s, p, n) ~ multinominal(phi(zeta(d, z(d, c, s, p, n))))
            }
          }
        }
      }
    }
  }

  val models = Seq(Unigram, MixtureOfUnigrams, LDA, PLDA, Deep)

  def main(args: Array[String]): Unit = {
    println(Model.toDot(models))
  }
}
