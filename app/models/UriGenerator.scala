package models

import java.util.UUID

import store.Namespace

trait UriGenerator[T <: UniqueEntity] {
  def base: String
  def generateUri(e: T)(implicit ns: Namespace): String = s"$ns/$base/${e.id}"
  def generateUri(id: UUID)(implicit ns: Namespace): String = s"$ns/$base/$id"
  def randomUri(implicit ns: Namespace): String = s"$ns$base/$randomUUID"
  def randomUUID: UUID = UUID.randomUUID()
  def splitter(implicit ns: Namespace): URLSplit[UUID] = new URLSplit[UUID] {
    override def from(url: String): UUID = UUID.fromString(url.split(s"$ns/$base/")(1))
    override def to(a: UUID): String = generateUri(a)
  }
}
