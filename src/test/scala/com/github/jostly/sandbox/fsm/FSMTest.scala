package com.github.jostly.sandbox.fsm

import java.time.ZonedDateTime

import org.scalatest.{FunSuite, Inside, Matchers}

class FSMTest extends FunSuite with Matchers with Inside {
  import MyStateMachine._

  test("building an order from events, extracting information") {
    val order = Nil
      .send(Request(Id("17")))
      .send(Enrich(Name("johan")))
      .send(Complete())

    order should have size 3

    order.id shouldBe Id("17")
    order.name shouldBe Some(Name("johan"))
  }

  test("extracting information from partial order") {
    val order = Nil
      .send(Request(Id("26")))

    order should have size 1

    order.id shouldBe Id("26")
    order.name shouldBe None
  }

  test("extracting information from Nil treated as an order") {
    Nil.name shouldBe None
    a[NoSuchElementException] should be thrownBy Nil.id
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

  implicit class Order(events: List[Event]) {
    def id: Id = events.collect { case Requested(id, _) => id }.head
    def name: Option[Name] = events.collect { case Enriched(name, _) => name }.headOption
  }

  implicit val `Requested provides identity`: ProvidesIdentity[Requested, Id] = new ProvidesIdentity[Requested, Id] {
    override def id(e: Requested): Id = e.id
  }

  implicit val machine: StateMachine[State, Command, Event] = StateMachine(
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
