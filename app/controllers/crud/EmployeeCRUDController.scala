package controllers.crud

import models.UriGenerator
import models.users.Employee
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import store.{Namespace, SesameRepository}

class EmployeeCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[Employee] {
   override implicit def rdfWrites: ToPG[Sesame, Employee] = defaultBindings.EmployeeBinding.employeeBinder

   override implicit def rdfReads: FromPG[Sesame, Employee] = defaultBindings.EmployeeBinding.employeeBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, Employee] = defaultBindings.EmployeeBinding.classUri

   override implicit def uriGenerator: UriGenerator[Employee] = Employee

   override implicit def reads: Reads[Employee] = Employee.reads

   override implicit def writes: Writes[Employee] = Employee.writes
 }
