package com.todesking.platebuilder

sealed abstract class HList
class HNil extends HList
class ::[A, B <: HList] extends HList

object HList {
  type HNel = `::`[_, _ <: HList]

  sealed abstract class Concat[A <: HList, B <: HList] {
    type Result <: HList
  }
  final class ConcatR[A <: HList, B <: HList, R <: HList] extends Concat[A, B] {
    override type Result = R
  }
  implicit def concat0[A <: HList]: ConcatR[HNil, A, A] = new ConcatR
  implicit def concat1[A, B <: HList, C <: HList, D <: HList](implicit ev: ConcatR[B, C, D]): ConcatR[A :: B, C, A :: D] = new ConcatR

  trait SetAdd[A <: HList, B] {
    type Result <: HNel
  }

  final class ContainsOnly[A <: HList, B]
  implicit def containsOnly0[A]: ContainsOnly[A :: HNil, A] = new ContainsOnly
  implicit def containsOnly1[A <: HList, B](implicit ev: ContainsOnly[A, B]): ContainsOnly[B :: A, B] = new ContainsOnly

  trait IsEmpty[A <: HList]

  trait RemoveAll[A <: B, B] {
    type Result <: HList
  }
}
