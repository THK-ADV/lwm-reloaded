package models

import java.util.UUID

import org.joda.time.DateTime

sealed trait Session

case class ValidSession(username: String, userId: UUID, id: UUID = UUID.randomUUID(), expirationDate: DateTime = DateTime.now().plusDays(1)) extends Session

case class InvalidSession(message: String) extends Session
