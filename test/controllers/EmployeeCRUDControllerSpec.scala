package controllers

import controllers.crud.{AbstractCRUDController, EmployeeCRUDController}
import models.users.Employee
import play.api.libs.json.Writes

class EmployeeCRUDControllerSpec extends AbstractCRUDControllerSpec[Employee] {
  override val entityToPass: Employee = Employee("system id to pass", "surname to pass", "forename to pass", "email to pass")

  override def entityTypeName: String = "Employee"

  override val controller: AbstractCRUDController[Employee] = new EmployeeCRUDController(repository, namespace)

  override val entityToFail: Employee = Employee("system id to fail", "surname to fail", "forename to fail", "email to fail")

  override implicit val jsonWrites: Writes[Employee] = Employee.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
