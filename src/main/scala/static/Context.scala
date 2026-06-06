package static

import lang.SecurityLevel.L
import lang.{Type, TypeAnnotation}
import lang.TypeAnnotation.Static

import scala.collection.mutable

type Env [A, B] = mutable.Map[A, B]

case class Context(
                    pc: TypeAnnotation = Static(L),
                    variableEnv: Env[String, Type] = mutable.Map(),
                    addressEnv: Env[String, Type] = mutable.Map()
                  ) {
  def withVariable(name: String, t: Type): Context =
    this.copy(variableEnv = this.variableEnv + (name -> t))

  def withAllocation(loc: String, t: Type): Context =
    this.copy(addressEnv = this.addressEnv + (loc -> t))

  def withPC(newPC: TypeAnnotation): Context =
    this.copy(pc = newPC)
}