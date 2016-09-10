package com.github.jostly.sandbox

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

package object fsm {

  def when[C, E, S, S1, S2](s: S1)(ops: Op[C, E, S2]*)(implicit ev1: S1 <:< S, ev2: S2 <:< S): List[(Option[S], Op[C, E, S])] = {
    ops.toList.map(x => (Some(ev1(s)), x.map(ev2)))
  }

  def whenIdle[C, E, S](ops: GotoOp[C, E, S]*): List[(Option[S], Op[C, E, S])] = {
    ops.toList.map(x => (None, x))
  }

  def on[C: TypeTag : ClassTag] = new On[C]

  class On[C: TypeTag : ClassTag] {

    def emit[E: TypeTag : ClassTag](fn: C => E) = new Emit(fn)

  }

  class Emit[C: TypeTag : ClassTag, E: TypeTag : ClassTag](fn: C => E) {

    val part: PartialFunction[Any, E] = {
      case c: C => fn(c)
    }

    def and[S: ClassTag](s: stay.type) = {
      new StayOp[C, E, S](part, { case e: E => println(s"Emit $e")}, { case Some(s: S) => s })
    }
    def and[S: TypeTag : ClassTag](g: goto[S]) = {
      new GotoOp[C, E, S](part, { case e: E => println(s"Emit $e")}, { case _ => g.next }, g.next)
    }
  }

  sealed trait StateChange
  case object stay extends StateChange
  case class goto[S](next: S) extends StateChange

  sealed trait Op[+C, +E, +S] {
    def emitFunc: PartialFunction[Any, E]
    def handleFunc: PartialFunction[Any, Unit]
    def stateFunc: PartialFunction[Option[Any], S]
    def map[S1](f: S => S1): Op[C, E, S1]
  }

  class StayOp[C: TypeTag : ClassTag, E: TypeTag : ClassTag, S](val emitFunc: PartialFunction[Any, E],
                                                                val handleFunc: PartialFunction[Any, Unit],
                                                                val stateFunc: PartialFunction[Option[Any], S]) extends Op[C, E, S] {
    override def toString = s"${typeTag[C].tpe} -> ${typeTag[E].tpe} -> stay"
    override def map[S1](f: (S) => S1): Op[C, E, S1] = new StayOp[C, E, S1](emitFunc, handleFunc, stateFunc.andThen(f))
  }

  class GotoOp[C: TypeTag : ClassTag, E: TypeTag : ClassTag, S](val emitFunc: PartialFunction[Any, E],
                                                                val handleFunc: PartialFunction[Any, Unit],
                                                                val stateFunc: PartialFunction[Option[Any], S],
                                                                g: S) extends Op[C, E, S] {
    override def toString = s"${typeTag[C].tpe} -> ${typeTag[E].tpe} -> goto $g"
    override def map[S1](f: (S) => S1): Op[C, E, S1] = new GotoOp[C, E, S1](emitFunc, handleFunc, stateFunc.andThen(f), f(g))
  }

  implicit class RichSender[E](root: List[E]) {
    def send[S, C, E1](cmd: C)(implicit cr: IsCommandReceiver[E, C, E1]): List[E1] = {
      cr.send(root, cmd)
    }
  }

  trait IsCommandReceiver[-Ein, -C, +EOut] {
    def send(state: List[Ein], command: C): List[EOut]
  }

}
