package models

import java.util.UUID

import store.Namespace

trait UriGenerator[T <: UniqueEntity] {
  def base: String
  def generateUri(e: T)(implicit ns: Namespace): String = s"$ns$base/${e.id}"
  def randomUri(implicit ns: Namespace): String = s"$ns$base/$randomUUID"
  def randomUUID: UUID = UUID.randomUUID()
}