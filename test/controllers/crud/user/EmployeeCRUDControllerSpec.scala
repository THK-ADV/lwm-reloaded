package controllers.crud.user

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.users.{Employee, EmployeeProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import store.SesameRepository
import utils.LwmMimeType

class EmployeeCRUDControllerSpec extends AbstractCRUDControllerSpec[EmployeeProtocol, Employee] {
  override val entityToPass: Employee = Employee("system id to pass", "surname to pass", "forename to pass", "email to pass", Employee.randomUUID)

  override def entityTypeName: String = "employee"

  override val controller: AbstractCRUDController[EmployeeProtocol, Employee] = new EmployeeCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: EmployeeProtocol, id: Option[UUID]): Employee = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Employee = Employee("system id to fail", "surname to fail", "forename to fail", "email to fail", Employee.randomUUID)

  override implicit val jsonWrites: Writes[Employee] = Employee.writes

  override val mimeType: LwmMimeType = LwmMimeType.employeeV1Json

  override val inputJson: JsValue = Json.obj(
    "systemId" -> entityToPass.systemId,
    "lastname" -> entityToPass.lastname,
    "firstname" -> entityToPass.firstname,
    "email" -> entityToPass.email
  )

  import ops._
  import bindings.EmployeeBinding._
  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val updateJson: JsValue = Json.obj(
    "systemId" -> s"${entityToPass.systemId} updated",
    "lastname" -> s"${entityToPass.lastname} updated",
    "firstname" -> s"${entityToPass.firstname} updated",
    "email" -> s"${entityToPass.email} updated"
  )
}
