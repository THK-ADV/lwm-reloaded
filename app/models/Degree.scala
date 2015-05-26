package models

import java.util.UUID

import store.Namespace

case class Degree(label: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Degree extends UriGenerator[Degree] {
  def generateUri(degree: Degree)(implicit ns: Namespace): String = s"${ns}degrees/${degree.id}"
}