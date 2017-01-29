package com.todesking.platebuilder

abstract class PlateBuilder {
  def modelID: String = getClass.getName
  lazy val model: Model = builder.build()

  protected implicit val builder = new Builder(modelID)
}
