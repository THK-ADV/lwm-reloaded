package controllers.crud

import java.util.UUID

import models.users.{Employee, EmployeeProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LwmMimeType

class EmployeeCRUDControllerSpec extends AbstractCRUDControllerSpec[EmployeeProtocol, Employee] {
  override val entityToPass: Employee = Employee("system id to pass", "surname to pass", "forename to pass", "email to pass", Employee.randomUUID)

  override def entityTypeName: String = "employee"

  override val controller: AbstractCRUDController[EmployeeProtocol, Employee] = new EmployeeCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: EmployeeProtocol, id: Option[UUID]): Employee = entityToPass
  }

  override val entityToFail: Employee = Employee("system id to fail", "surname to fail", "forename to fail", "email to fail", Employee.randomUUID)

  override implicit val jsonWrites: Writes[Employee] = Employee.writes

  override val mimeType: LwmMimeType = LwmMimeType.employeeV1Json

  override val inputJson: JsValue = Json.obj(
      "systemId" -> "systemId input",
      "lastname" -> "lastname input",
      "firstname" -> "firstname input",
      "email" -> "email input"
    )

  import ops._
  import bindings.EmployeeBinding._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}
