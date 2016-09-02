package models

import java.util.UUID

import org.joda.time.DateTime

sealed trait Session {
  def fold[B](f: ValidSession => B)(g: InvalidSession => B): B = this match {
    case valid: ValidSession => f(valid)
    case invalid: InvalidSession => g(invalid)
  }
}

case class ValidSession(username: String, userId: UUID, id: UUID = UUID.randomUUID(), expirationDate: DateTime = DateTime.now().plusDays(1)) extends Session

case class InvalidSession(message: String) extends Session
