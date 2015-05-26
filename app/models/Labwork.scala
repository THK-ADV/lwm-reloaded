package models

import java.util.UUID

import store.Namespace

case class Labwork(label: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Labwork extends UriGenerator[Labwork] {
  def generateUri(labwork: Labwork)(implicit ns: Namespace): String = s"${ns}labworks/${labwork.id}"
}