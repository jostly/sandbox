package com.github.jostly.sandbox

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

package object fsm {

  import Internal._

  def when[C, E, S, S1, S2](s: S1)(ops: OpAndTransition[C, E, S2]*)(implicit ev1: S1 <:< S, ev2: S2 <:< S): List[(Option[S], Op[C, E, S])] = {
    ops.toList.map {
      case StayOp(a, b) =>
        (Some(ev1(s)), Op[C, E, S](a, b.andThen(_ => ev1(s))))
      case GotoOp(a, b, t) =>
        (Some(ev1(s)), Op[C, E, S](a, b.andThen(_ => ev2(t))))
    }
  }

  def whenIdle[C, E, S, I, S1](ops: GotoOp[C, E, S1]*)(implicit ev: ProvidesIdentity[E, I], ev1: S1 <:< S): List[(Option[S], Op[C, E, S])] = {
    ops.toList.map { op =>
      (None, Op[C, E, S](op.emitFunc, op.handleFunc.andThen(_ => ev1(op.target))))
    }
  }

  def on[C: ClassTag] = new On[C]

  sealed trait StateChange
  case object stay extends StateChange
  case class goto[S](next: S) extends StateChange

  object Internal {
    class On[C: ClassTag] {

      def emit[E: ClassTag](fn: C => E) = new Emit(fn)

    }

    class Emit[C: ClassTag, E: ClassTag](fn: C => E) {

      val part: PartialFunction[Any, E] = {
        case c: C => fn(c)
      }

      def and[S](s: stay.type) = {
        new StayOp[C, E, S](part, { case e: E => println(s"Emit $e") })
      }
      def and[S](g: goto[S]) = {
        new GotoOp[C, E, S](part, { case e: E => println(s"Emit $e") }, g.next)
      }
    }

    sealed trait OpAndTransition[+C, +E, +S] {
      def emitFunc: PartialFunction[Any, E]
      def handleFunc: PartialFunction[Any, Unit]
    }

    case class StayOp[+C, +E, +S](emitFunc: PartialFunction[Any, E],
                                  handleFunc: PartialFunction[Any, Unit]) extends OpAndTransition[C, E, S]

    case class GotoOp[+C, +E, +S](emitFunc: PartialFunction[Any, E],
                                  handleFunc: PartialFunction[Any, Unit],
                                  target: S) extends OpAndTransition[C, E, S]

  }

  case class Op[+C, +E, +S](emitFunc: PartialFunction[Any, E],
                            handleFunc: PartialFunction[Any, S])

  implicit class RichSender[E](root: List[E]) {
    def send[S, C, E1](cmd: C)(implicit cr: IsCommandReceiver[E, C, E1]): List[E1] = {
      cr.send(root, cmd)
    }
  }

  trait IsCommandReceiver[-Ein, -C, +EOut] {
    def send(state: List[Ein], command: C): List[EOut]
  }

  @implicitNotFound(msg = "Cannot prove that ${E} provides identity of type ${I}.")
  trait ProvidesIdentity[E, I] {
    def id(e: E): I
  }

}
