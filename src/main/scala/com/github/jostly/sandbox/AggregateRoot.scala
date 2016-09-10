package com.github.jostly.sandbox

import com.github.jostly.sandbox.fsm.ProvidesIdentity

import scala.reflect.ClassTag

trait AggregateRoot[Event] {
  val events: List[Event]
}

trait Identifiable[Id] { this: AggregateRoot[_] =>

  def id[E : ClassTag](implicit ev: ProvidesIdentity[E, Id]): Id =
    events.collect { case e: E => ev.id(e) }.head

}