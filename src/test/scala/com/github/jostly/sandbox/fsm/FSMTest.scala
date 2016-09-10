package com.github.jostly.sandbox.fsm

import java.time.ZonedDateTime

import com.github.jostly.sandbox.{AggregateRoot, Identifiable}
import org.scalatest.{FunSuite, Inside, Matchers}

class FSMTest extends FunSuite with Matchers with Inside {

  import MyStateMachine._

  test("building an order from events, extracting information") {
    val order = Nil
      .send(Request(Id("17")))
      .send(SetName(Name("johan")))
      .send(SetAddress(Address("localhost")))
      .send(Complete())

    order should have size 4

    order.id shouldBe Id("17")
    order.name shouldBe Some(Name("johan"))
    order.address shouldBe Some(Address("localhost"))
  }

  test("extracting information from partial order") {
    val order = Nil
      .send(Request(Id("26")))

    order should have size 1

    order.id shouldBe Id("26")
    order.name shouldBe None
    order.address shouldBe None
  }

  test("extracting information from Nil treated as an order") {
    Nil.name shouldBe None
    Nil.address shouldBe None
    a[NoSuchElementException] should be thrownBy Nil.id
  }


}

case class Id(value: String)
case class Name(value: String)
case class Address(value: String)

sealed trait Command
case class Request(id: Id) extends Command
case class SetName(name: Name) extends Command
case class SetAddress(address: Address) extends Command
case class Complete() extends Command
case class Fail() extends Command

import java.time.ZonedDateTime.now

sealed trait Event
case class Requested(id: Id, timestamp: ZonedDateTime = now()) extends Event
case class Enriched(name: Option[Name] = None, address: Option[Address] = None, timestamp: ZonedDateTime = now()) extends Event
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

  implicit class Order(val events: List[Event]) extends AggregateRoot[Event] with Identifiable[Id] {

    def name: Option[Name] = events.collect { case Enriched(Some(name), _, _) => name }.headOption
    def address: Option[Address] = events.collect { case Enriched(_, Some(address), _) => address }.headOption
  }

  implicit val `Requested provides identity`: ProvidesIdentity[Requested, Id] = new ProvidesIdentity[Requested, Id] {
    override def id(e: Requested): Id = e.id
  }

  implicit val machine: StateMachine[State, Command, Event] = StateMachine(
    whenIdle(
      on[Request] emit (c => Requested(c.id)) and goto(State.Requested)
    ),
    when(State.Requested)(
      on[SetName] emit (c => Enriched(name = Some(c.name))) and goto(State.Enriched),
      on[SetAddress] emit (c => Enriched(address = Some(c.address))) and goto(State.Enriched)
    ),
    when(State.Enriched)(
      on[SetName] emit (c => Enriched(name = Some(c.name))) and stay,
      on[SetAddress] emit (c => Enriched(address = Some(c.address))) and stay,
      on[Complete] emit (_ => Completed()) and goto(State.Completed),
      on[Fail] emit (_ => Failed()) and goto(State.Failed)
    )
  )
}
