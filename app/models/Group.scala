package models

import java.util.UUID

import store.Namespace

case class Group(groupSchedule: String, label: String, labwork: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

// not included members: []

object Group extends UriGenerator[Group] {
  def generateUri(group: Group)(implicit ns: Namespace): String = s"${ns}groups/${group.id}"
}
