package controllers

import controllers.crud.{StudentCRUDController, AbstractCRUDController}
import models.users.Student
import play.api.libs.json.Writes

class StudentCRUDControllerSpec extends AbstractCRUDControllerSpec[Student] {
  override val entityToPass: Student = Student("system id to pass", "surname to pass", "forename to pass", "email to pass", "registration id to pass")

  override def entityTypeName: String = "Student"

  override val controller: AbstractCRUDController[Student] = new StudentCRUDController(repository, namespace)

  override val entityToFail: Student = Student("system id to fail", "surname to fail", "forename to fail", "email to fail", "registration id to fail")

  override implicit val jsonWrites: Writes[Student] = Student.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
