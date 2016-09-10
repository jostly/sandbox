package com.github.jostly.sandbox.fsm

trait StateMachine[State, Cmd, Evt] {
  def send(state: List[Evt], command: Cmd): List[Evt] = {
    val s = state.foldRight(None.asInstanceOf[Option[State]])(replayFunc)
    send(s, command) :: state
  }

  def replayFunc(e: Evt, s: Option[State]): Option[State] = {
    Some(handle(s, e))
  }

  def send(state: Option[State], command: Cmd): Evt
  def handle(state: Option[State], event: Evt): State
}

object StateMachine {
  def apply[State, Cmd, Evt](opsies: List[(Option[State], Op[Cmd, Evt, State])]*): StateMachine[State, Cmd, Evt] =
    new Impl[State, Cmd, Evt](opsies
      .toList
      .flatten
      .groupBy(e => e._1)
      .mapValues(e => e.map(x => x._2))
      .toList
    )

  class Impl[S, C, E](val operations: List[(Option[S], List[Op[C, E, S]])]) extends StateMachine[S, C, E] {
    override def send(state: Option[S], command: C): E = {
      operations.find(_._1 == state) match {
        case Some((_, ops)) =>
          ops.find(op => op.emitFunc.isDefinedAt(command)) match {
            case Some(op) =>
              val e = op.emitFunc(command)
              println(s"Sent $command, got $e")
              e
            case None => throw new IllegalStateException(s"No action for $command in $state")
          }
        case None => throw new IllegalStateException(s"No actions in $state")
      }
    }
    override def handle(state: Option[S], event: E): S = {
      operations.find(_._1 == state) match {
        case Some((_, ops)) =>
          ops.find(op => op.handleFunc.isDefinedAt(event)) match {
            case Some(op) =>
              op.handleFunc(event)
              op.stateFunc(state)
            case None =>
              throw new IllegalStateException(s"No handler for $event in $state")
          }
        case None => throw new IllegalStateException(s"No event handlers in $state")
      }
    }
  }

}

