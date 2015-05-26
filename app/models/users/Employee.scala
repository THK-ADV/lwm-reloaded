package models.users

import java.util.UUID

import models._
import store.Namespace

case class Employee(systemId: String,
                    lastname: String,
                    firstname: String,
                    email: String,
                    id: UUID = UUID.randomUUID()) extends User

object Employee extends UriGenerator[Employee] {
  def generateUri(employee: Employee)(implicit ns: Namespace): String = s"${ns}employees/${employee.id}"
}
