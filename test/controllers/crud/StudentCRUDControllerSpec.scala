package controllers.crud

import java.util.UUID

import models.users.{Student, StudentProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LWMMimeType

class StudentCRUDControllerSpec extends AbstractCRUDControllerSpec[StudentProtocol, Student] {
  override val entityToPass: Student = Student("system id to pass", "surname to pass", "forename to pass", "email to pass", "registration id to pass", Student.randomUUID)

  override def entityTypeName: String = "student"

  override val controller: AbstractCRUDController[StudentProtocol, Student] = new StudentCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: StudentProtocol, id: Option[UUID]): Student = entityToPass
  }

  override val entityToFail: Student = Student("system id to fail", "surname to fail", "forename to fail", "email to fail", "registration id to fail", Student.randomUUID)

  override implicit val jsonWrites: Writes[Student] = Student.writes

  override val mimeType: LWMMimeType = LWMMimeType.studentV1Json

  override val inputJson: JsValue = Json.obj(
    "systemId" -> "systemId input",
    "lastname" -> "lastname input",
    "firstname" -> "firstname input",
    "email" -> "email input",
    "registrationId" -> "registrationId input"
  )

  import bindings.StudentBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}

