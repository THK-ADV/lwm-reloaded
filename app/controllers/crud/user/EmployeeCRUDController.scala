package controllers.crud.user

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions._
import models.users.{Employee, EmployeeProtocol}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class EmployeeCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[EmployeeProtocol, Employee] {
   override implicit def rdfWrites: ToPG[Sesame, Employee] = defaultBindings.EmployeeBinding.employeeBinder

   override implicit def rdfReads: FromPG[Sesame, Employee] = defaultBindings.EmployeeBinding.employeeBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, Employee] = defaultBindings.EmployeeBinding.classUri

   override implicit def uriGenerator: UriGenerator[Employee] = Employee

   override implicit def reads: Reads[EmployeeProtocol] = Employee.reads

   override implicit def writes: Writes[Employee] = Employee.writes

   override protected def fromInput(input: EmployeeProtocol, existing: Option[Employee]): Employee = existing match {
      case Some(employee) => Employee(input.systemId, input.lastname, input.firstname, input.email, employee.id)
      case None => Employee(input.systemId, input.lastname, input.firstname, input.email, Employee.randomUUID)
   }

   override val mimeType: LwmMimeType = LwmMimeType.employeeV1Json

   override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Employee]): Try[Set[Employee]] = Success(all)

   override protected def atomize(output: Employee): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

   override protected def atomizeMany(output: Set[Employee]): Try[JsValue] = Success(Json.toJson(output))

   override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case Get => PartialSecureBlock(user.get)
      case GetAll => PartialSecureBlock(user.getAll)
      case _ => PartialSecureBlock(god)
   }

   override protected def compareModel(input: EmployeeProtocol, output: Employee): Boolean = {
      input.firstname == output.firstname && input.lastname == input.lastname && input.email == output.email
   }
}
