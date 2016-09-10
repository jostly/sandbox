package com.github.jostly.sandbox.fsm

import java.time.ZonedDateTime

import org.scalatest.{FunSuite, Inside, Matchers}

class FSMTest extends FunSuite with Matchers with Inside {

  test("it") {
    import MyStateMachine._
    val stream = Nil
      .send(Request(Id("17")))
      .send(Enrich(Name("johan")))
      .send(Complete())
    println("Event stream for complete order: " + stream)
  }

}

case class Id(value: String) extends AnyVal
case class Name(value: String) extends AnyVal

sealed trait Command
case class Request(id: Id) extends Command
case class Enrich(name: Name) extends Command
case class Complete() extends Command
case class Fail() extends Command

import java.time.ZonedDateTime.now

sealed trait Event
case class Requested(id: Id, timestamp: ZonedDateTime = now()) extends Event
case class Enriched(name: Name, timestamp: ZonedDateTime = now()) extends Event
case class Completed(timestamp: ZonedDateTime = now()) extends Event
case class Failed(timestamp: ZonedDateTime = now()) extends Event

sealed trait State
object State {
  case object Requested extends State
  case object Enriched extends State
  case object Completed extends State
  case object Failed extends State
}

object MyStateMachine {

  implicit val `MyStateMachine receives commands`: IsCommandReceiver[Event, Command, Event] = new IsCommandReceiver[Event, Command, Event] {
    override def send(state: List[Event], command: Command): List[Event] = machine.send(state, command)
  }

  implicit val `Requested provides identity`: ProvidesIdentity[Requested, Id] = new ProvidesIdentity[Requested, Id] {
    override def id(e: Requested): Id = e.id
  }

  val machine: StateMachine[State, Command, Event] = StateMachine(
    whenIdle(
      on[Request] emit (c => Requested(c.id)) and goto(State.Requested)
    ),
    when(State.Requested)(
      on[Enrich] emit (c => Enriched(c.name)) and goto(State.Enriched)
    ),
    when(State.Enriched)(
      on[Enrich] emit (c => Enriched(c.name)) and stay,
      on[Complete] emit (_ => Completed()) and goto(State.Completed),
      on[Fail] emit (_ => Failed()) and goto(State.Failed)
    )
  )
}
