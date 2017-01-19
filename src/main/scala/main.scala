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
