package com.github.jostly.sandbox

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

package object fsm {

  import Internal._

  def when[C, E, S, S1, S2](s: S1)(ops: OpAndTransition[C, E, S2]*)(implicit ev1: S1 <:< S, ev2: S2 <:< S): List[(Option[S], Op[C, E, S])] = {
    when(s :: Nil)(ops: _*)
  }

  def when[C, E, S, S1, S2](xs: List[S1])(ops: OpAndTransition[C, E, S2]*)(implicit ev1: S1 <:< S, ev2: S2 <:< S): List[(Option[S], Op[C, E, S])] = {
    ops.toList.flatMap {
      case StayOp(a, b, cd, ed) =>
        xs.map(s => (Some(ev1(s)), Op[C, E, S](a, b.andThen(_ => ev1(s)), cd, ed, s.toString)))
      case GotoOp(a, b, cd, ed, t) =>
        xs.map(s => (Some(ev1(s)), Op[C, E, S](a, b.andThen(_ => ev2(t)), cd, ed, t.toString)))
    }
  }

  def whenIdle[C, E, S, I, S1](ops: GotoOp[C, E, S1]*)(implicit ev: ProvidesIdentity[E, I], ev1: S1 <:< S): List[(Option[S], Op[C, E, S])] = {
    ops.toList.map { op =>
      (None, Op[C, E, S](op.emitFunc, op.handleFunc.andThen(_ => ev1(op.target)), op.commandDescriptor, op.eventDescriptor, op.target.toString))
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
        new And({ case e: E => }).and(s)
      }
      def and[S](g: goto[S]) = {
        new And({ case e: E => }).and(g)
      }
      def to(fn: E => Unit) = {
        new And({ case e: E => fn(e) })
      }

      class And(fn: PartialFunction[Any, Unit]) {
        def and[S](s: stay.type) = {
          new StayOp[C, E, S](part, fn, classTag[C], classTag[E])
        }
        def and[S](g: goto[S]) = {
          new GotoOp[C, E, S](part, fn, classTag[C], classTag[E], g.next)
        }
      }
    }

    implicit def prettyName[C](ct: ClassTag[C]): String =
      ct.runtimeClass.getSimpleName.replaceFirst("""\$$""", "")

    sealed trait OpAndTransition[+C, +E, +S] {
      def emitFunc: PartialFunction[Any, E]
      def handleFunc: PartialFunction[Any, Unit]
    }

    case class StayOp[+C, +E, +S](emitFunc: PartialFunction[Any, E],
                                  handleFunc: PartialFunction[Any, Unit],
                                  commandDescriptor: String,
                                  eventDescriptor: String) extends OpAndTransition[C, E, S]

    case class GotoOp[+C, +E, +S](emitFunc: PartialFunction[Any, E],
                                  handleFunc: PartialFunction[Any, Unit],
                                  commandDescriptor: String,
                                  eventDescriptor: String,
                                  target: S) extends OpAndTransition[C, E, S]

  }

  case class Op[+C, +E, +S](emitFunc: PartialFunction[Any, E],
                            handleFunc: PartialFunction[Any, S],
                            commandDescriptor: String,
                            eventDescriptor: String,
                            targetStateDescriptor: String)


  implicit class RichSingleState[S](s1: S) {
    def |[T >: S](s2: T): List[T] = s2 :: s1 :: Nil
  }

  implicit class RichStateList[S](xs: List[S]) {
    def |[T >: S](sn: T): List[T] = sn :: xs
  }

  implicit class RichSender[E](root: List[E]) {
    def send[S, C, E1](cmd: C)(implicit cr: CommandReceiver[E, C, E1]): List[E1] = {
      cr.send(root, cmd)
    }
  }

  @implicitNotFound(msg = "Cannot prove that ${E} provides identity of type ${I}.")
  trait ProvidesIdentity[E, I] {
    def id(e: E): I
  }

}
