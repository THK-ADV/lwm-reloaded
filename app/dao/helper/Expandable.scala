package dao.helper

import models.UniqueDbEntity

trait Expandable[DbModel <: UniqueDbEntity] {
  protected def databaseExpander: Option[DatabaseExpander[DbModel]] = None
}
