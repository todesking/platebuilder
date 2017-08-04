import com.todesking.platebuilder.{ PlateBuilder, Model }

object Demo {
  object Unigram extends PlateBuilder {
    import builder.dsl._

    val V = size.V("Number of vocabularies")
    val D = size.D("Number of documents")
    val N = size.N("Number of words for each documents") * D

    // hyperparameters
    val beta = given.beta.realVec(V)

    // variables
    val phi = hidden.phi.realVec(V)
    val w = observed.w.category(V) * (D, N)

    phi ~ dirichlet(beta)

    for (d <- D) {
      for (n <- N(d)) {
        w(d, n) ~ categorical(phi)
      }
    }
  }

  val MixtureOfUnigrams = Model.define("MixtureOfUnigrams") { implicit builder =>
    import builder.dsl._
    val K = size.K("Number of topics")
    val V = size.V("Number of vocabularies")
    val D = size.D("Number of documents")
    val N = size.N("Number of words for each documents") * D

    // hyperparameters
    val alpha = given.alpha.realVec(K)
    val beta = given.beta.realVec(V)

    // variables
    val phi = hidden.phi.realVec(V) * K
    val theta = hidden.theta.realVec(K)
    val z = hidden("z", "Hidden topic for each document").category(K) * D
    val w = observed.w.category(V) * (D, N)

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    theta ~ dirichlet(alpha)

    for (d <- D) {
      z(d) ~ categorical(theta)
      for (n <- N(d)) {
        w(d, n) ~ categorical(phi(z(d)))
      }
    }
  }

  val LDA = Model.define("LDA") { implicit builder =>
    import builder.dsl._
    val K = size.K("Number of topics")
    val V = size.V("Number of vocabularies")
    val D = size.D("Number of documents")
    val N = size.N("Number of words for each documents") * D

    // hyperparameters
    val alpha = given.alpha.realVec(K)
    val beta = given.beta.realVec(V)

    // variables
    val phi = hidden.phi.realVec(V) * K
    val theta = hidden.theta.realVec(K) * D
    val z = hidden("z", "Hidden topic for each word").category(K) * (D, N)
    val w = observed.w.category(V) * (D, N)

    for (k <- K) {
      phi(k) ~ dirichlet(beta)
    }

    for (d <- D) {
      theta(d) ~ dirichlet(alpha)
      for (n <- N(d)) {
        z(d, n) ~ categorical(theta(d))
        w(d, n) ~ categorical(phi(z(d, n)))
      }
    }
  }

  val basic = Seq(Unigram.model, MixtureOfUnigrams, LDA)

  def main(args: Array[String]): Unit = {
    println(Model.toDot(basic))
  }
}
