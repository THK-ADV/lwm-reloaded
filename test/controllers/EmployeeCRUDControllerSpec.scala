package controllers

import java.util.UUID

import controllers.crud.{AbstractCRUDController, EmployeeCRUDController}
import models.users.{EmployeeProtocol, Employee}
import play.api.libs.json.{Json, JsValue, Writes}

class EmployeeCRUDControllerSpec extends AbstractCRUDControllerSpec[EmployeeProtocol, Employee] {
  override val entityToPass: Employee = Employee("system id to pass", "surname to pass", "forename to pass", "email to pass", Employee.randomUUID)

  override def entityTypeName: String = "Employee"

  override val controller: AbstractCRUDController[EmployeeProtocol, Employee] = new EmployeeCRUDController(repository, namespace) {
    override protected def fromInput(input: EmployeeProtocol, id: Option[UUID]): Employee = entityToPass
  }

  override val entityToFail: Employee = Employee("system id to fail", "surname to fail", "forename to fail", "email to fail", Employee.randomUUID)

  override implicit val jsonWrites: Writes[Employee] = Employee.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override val inputJson: JsValue = Json.obj(
      "systemId" -> "systemId input",
      "lastname" -> "lastname input",
      "firstname" -> "firstname input",
      "email" -> "email input"
    )
}
