package models

import java.util.UUID

import store.Namespace

trait UriGenerator[T <: UniqueEntity] {
  def base: String
  def generateUri(e: T)(implicit ns: Namespace): Option[String] = e.id.map(id => s"${ns}$base/${id}")
  def randomUri(implicit ns: Namespace): String = s"${ns}$base/${UUID.randomUUID()}"
  def randomUUID: UUID = UUID.randomUUID()
}