package com.todesking.platebuilder

sealed abstract class Observation
object Observation {
  case object Hidden extends Observation
  case object Observed extends Observation
  case object Given extends Observation
}
