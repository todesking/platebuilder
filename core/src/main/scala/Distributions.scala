package com.todesking.platebuilder

trait Distributions {
  import DSL.GeneratorSyntax

  def dirichlet[I <: String](param: Var[Type.Vec[I, Type.Real]]): Generator[Type.Vec[I, Type.Real]] =
    stochastic"Dirichlet($param)"

  def categorical[I <: String](param: Var[Type.Vec[I, Type.Real]]): Generator[Type.Category[I]] =
    stochastic"Categorical($param)"

  def normal(mu: Var[Type.Real], s2: Var[Type.Real]): Generator[Type.Real] =
    stochastic"Normal($mu, $s2)"
}
