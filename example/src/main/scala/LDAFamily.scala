import com.todesking.platebuilder.{ PlateBuilder, Model }

object LDAFamily {
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

  val PLDA = Model.define("PLDA") { implicit builder =>
    import builder.dsl._
    val K = size.K("Num of topics")
    val V = size.V("Num of vocabularies")
    val D = size.D("Num of documents")
    val N = size.N("Num of words for each document") * D
    val L = size.L("Num of labels")
    val Kd = computed("Kd", "Num of labels assigned for each document").size * D
    val K_L = size("K_L", "Num of topics assigned for each document and label") * (D, Kd)

    // hyperparameters
    val alpha = given.alpha.R
    val beta = given.beta.realVec(V)

    val alphaL = computed("α_L").realVec(Kd(D)) * D
    val alphaT = computed("α_T").realVec(K) * (D, Kd)

    // variables
    val phi = hidden.phi.realVec(V) * K
    val theta = hidden.theta("Topic distribution").realVec(K) * (D, Kd)
    val Lambda = observed.Lambda("Assigned labels").binaryVec(L) * D
    val psi = hidden.psi.realVec(Kd(D)) * D
    val z = hidden.z.category(K) * (D, N)
    val w = observed.w.category(V) * (D, N)
    val l = hidden.l.category(Kd(D)) * (D, N)

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
        l(d, n) ~ categorical(psi(d))
        z(d, n) ~ categorical(theta(d, l(d, n)))
        w(d, n) ~ categorical(phi(z(d, n)))
      }
    }
  }

  val basic = Seq(Unigram.model, MixtureOfUnigrams, LDA)
  val all = Seq(Unigram, MixtureOfUnigrams, LDA, PLDA)

  def main(args: Array[String]): Unit = {
    println(Model.toDot(basic))
  }
}
