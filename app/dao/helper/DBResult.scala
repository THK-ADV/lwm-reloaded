package dao.helper

import models.UniqueDbEntity

sealed trait DBResult[A <: UniqueDbEntity] {
  def entity: A
}

object DBResult {

  case class Created[A <: UniqueDbEntity](entity: A) extends DBResult[A]

  case class Updated[A <: UniqueDbEntity](entity: A) extends DBResult[A]

}


