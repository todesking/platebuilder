package com.todesking.platebuilder

sealed abstract class HList
class HNil extends HList
class ::[A, B <: HList] extends HList
