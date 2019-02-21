package dao.helper

import models.UniqueDbEntity

sealed trait DBResult[A <: UniqueDbEntity] { // TODO use this instead of Option[A] for create or update queries
  def entity: A
}

case class Created[A <: UniqueDbEntity](entity: A) extends DBResult[A]

case class Updated[A <: UniqueDbEntity](entity: A) extends DBResult[A]


