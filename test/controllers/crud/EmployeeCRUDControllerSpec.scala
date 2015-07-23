package controllers.crud

import java.util.UUID

import models.users.{Employee, EmployeeProtocol}
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LWMMimeType

class EmployeeCRUDControllerSpec extends AbstractCRUDControllerSpec[EmployeeProtocol, Employee] {
  override val entityToPass: Employee = Employee("system id to pass", "surname to pass", "forename to pass", "email to pass", Employee.randomUUID)

  override def entityTypeName: String = "employee"

  override val controller: AbstractCRUDController[EmployeeProtocol, Employee] = new EmployeeCRUDController(repository, namespace) {
    override protected def fromInput(input: EmployeeProtocol, id: Option[UUID]): Employee = entityToPass
  }

  override val entityToFail: Employee = Employee("system id to fail", "surname to fail", "forename to fail", "email to fail", Employee.randomUUID)

  override implicit val jsonWrites: Writes[Employee] = Employee.writes

  override val mimeType: LWMMimeType = LWMMimeType.employeeV1Json

  override val inputJson: JsValue = Json.obj(
      "systemId" -> "systemId input",
      "lastname" -> "lastname input",
      "firstname" -> "firstname input",
      "email" -> "email input"
    )
}
