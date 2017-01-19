package com.todesking.platebuilder

sealed abstract class Type
object Type {
  type Real = Real.type
  case object Real extends Type
  case class Size[A <: String]() extends Type
  case class Category[A <: String](size: Size[A]) extends Type
  case class Vec[I <: String, A <: Type](elm: A) extends Type {
    def elementType: A = elm
  }
}

