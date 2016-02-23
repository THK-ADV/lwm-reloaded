package controllers.crud.user

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.Degree
import models.users.{Student, StudentProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import store.SesameRepository
import store.sparql.{NoneClause, select, Clause}
import utils.LwmMimeType

class StudentCRUDControllerSpec extends AbstractCRUDControllerSpec[StudentProtocol, Student] {
  override val entityToPass: Student = Student("system id to pass", "surname to pass", "forename to pass", "email to pass", "registration id to pass", Degree.randomUUID, Student.randomUUID)

  override def entityTypeName: String = "student"

  override val controller: AbstractCRUDController[StudentProtocol, Student] = new StudentCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: StudentProtocol, id: Option[UUID]): Student = entityToPass
  }

  override val entityToFail: Student = Student("system id to fail", "surname to fail", "forename to fail", "email to fail", "registration id to fail", Degree.randomUUID, Student.randomUUID)

  override implicit val jsonWrites: Writes[Student] = Student.writes

  override val mimeType: LwmMimeType = LwmMimeType.studentV1Json

  override val inputJson: JsValue = Json.obj(
    "systemId" -> entityToPass.systemId,
    "lastname" -> entityToPass.lastname,
    "firstname" -> entityToPass.firstname,
    "email" -> entityToPass.email,
    "registrationId" -> entityToPass.registrationId,
    "enrollment" -> entityToPass.enrollment
  )

  import ops._
  import bindings.StudentBinding._
  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val updateJson: JsValue = Json.obj(
    "systemId" -> s"${entityToPass.systemId} updated",
    "lastname" -> s"${entityToPass.lastname} updated",
    "firstname" -> s"${entityToPass.firstname} updated",
    "email" -> s"${entityToPass.email} updated",
    "registrationId" -> s"${entityToPass.registrationId} updated",
    "enrollment" -> entityToPass.enrollment
  )
}

