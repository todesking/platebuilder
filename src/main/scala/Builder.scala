package com.todesking.platebuilder

class Builder(id: String) { self =>
  import scala.collection.mutable

  private[this] var vars: Map[VarID, Var[_ <: Type]] = Map()
  private[this] var varOrder: Seq[VarID] = Seq()
  private[this] var indices: Map[VarID, Seq[IndexID]] = Map()
  private[this] val inEdges: mutable.MultiMap[VarID, VarID] =
    new mutable.HashMap[VarID, mutable.Set[VarID]] with mutable.MultiMap[VarID, VarID]
  private[this] var generators: Map[VarID, Generator[_ <: Type]] = Map()
  private[this] var descs: Map[VarID, String] = Map()
  private[this] var observations: Map[VarID, Observation] = Map()

  private[this] var _currentPath: Seq[IndexID] = Seq()

  def build(): Model = {
    val missingGenerators = vars.keys.filterNot { id => generators.contains(id) }
    if (missingGenerators.nonEmpty) {
      throw new IllegalStateException(s"Generator undefined: ${missingGenerators.map(_.str).mkString(", ")}")
    }
    val undefinedVars = (
      indices.keySet ++ indices.values.flatten.map(id => VarID(id.str)).toSet ++ inEdges.keySet ++ inEdges.values.flatten.toSet ++ generators.keySet ++ descs.keySet
    ).filterNot { id => vars.contains(id) }
    if (undefinedVars.nonEmpty) {
      throw new IllegalStateException(s"Unknown variables: ${undefinedVars.map(_.str).mkString(", ")}")
    }
    new Model(
      id,
      varOrder,
      vars,
      indices,
      inEdges.toMap.mapValues(_.toSet),
      generators,
      observations,
      descs.toMap
    )
  }

  def newVar(v: Var[_ <: Type], observation: Option[Observation], description: Option[String]): Unit = {
    setVar(v)
    description.foreach(setDescription(v.id, _))
    observation match {
      case Some(Observation.Given) =>
        setGenerator(v, Seq(), Generator.Given(), false)
      case Some(Observation.Observed) =>
        setGenerator(v, Seq(), Generator.Given(), false)
      case Some(_) | None =>
    }
    observation.foreach(setObservation(v.id, _))
  }

  def newConst(v: Var[_ <: Type], repr: String): Unit = {
    setVar(v)
    setObservation(v.id, Observation.Given)
    setGenerator(v, Seq(), Generator.Const(repr), true)
  }

  private[this] def setVar(v: Var[_ <: Type]): Unit = {
    if (!vars.contains(v.id)) {
      varOrder :+= v.id
    }
    vars += (v.id -> v)
  }

  private[this] def setDescription(vid: VarID, desc: String): Unit = {
    descs += (vid -> desc)
  }

  private[this] def setObservation(v: VarID, o: Observation): Unit = {
    observations += (v -> o)
  }

  def addIndex(id: VarID, i: Seq[IndexID]): Unit = {
    indices.get(id).fold {
      indices += (id -> i)
    } { is =>
      indices += (id -> (i ++ is))
    }
  }

  def withIndex[A](i: IndexID)(f: => A): A = {
    _currentPath :+= i
    val ret = f
    _currentPath = _currentPath.take(_currentPath.size - 1)
    ret
  }

  def setGenerator(v: Var[_ <: Type], path: Seq[IndexID], g: Generator[_ <: Type], ignoreCurrentPath: Boolean): Unit = {
    setVar(v)
    inEdges.remove(v.id)
    if (!ignoreCurrentPath && path != _currentPath) {
      val p1 = _currentPath.map(_.str).mkString("(", ", ", ")")
      val p2 = path.map(_.str).mkString("(", ", ", ")")
      throw new RuntimeException(s"Path not match for ${v.id.str}: expected ${p1} but ${p2}")
    }
    indices += (v.id -> path)
    g.dependencies.foreach { d =>
      inEdges.addBinding(v.id, d)
    }
    generators += (v.id -> g)
  }

  val dsl = new DSL(self)
}
object Builder {
  class Mapping[A <: String, B <: String](t1: Type.Size[A], t2: Type.Size[B]) extends Function1[Var[Type.Category[A]], Var[Type.Category[B]]] {
    override def apply(a: Var[Type.Category[A]]): Var[Type.Category[B]] = new Var.Simple(a.id, new Type.Category(t2))
  }

}
