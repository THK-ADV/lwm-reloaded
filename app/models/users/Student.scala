package models.users

import java.util.UUID

import models._
import store.Namespace

case class Student(systemId: String,
                   lastname: String,
                   firstname: String,
                   email: String,
                   registrationId: String, id: UUID = UUID.randomUUID()) extends User

object Student extends UriGenerator[Student] {
  def generateUri(student: Student)(implicit ns: Namespace): String = s"${ns}students/${student.id}"
}
