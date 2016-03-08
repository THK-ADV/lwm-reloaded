package controllers.security

import java.util.UUID

import controllers.crud._
import models.UriGenerator
import models.security.Permissions._
import models.security._
import models.users.{Student, Employee, User}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class AuthorityController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[AuthorityProtocol, Authority] {

  override implicit def rdfReads: FromPG[Sesame, Authority] = defaultBindings.AuthorityBinding.authorityBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Authority] = defaultBindings.AuthorityBinding.classUri

  override implicit def uriGenerator: UriGenerator[Authority] = Authority

  override implicit def rdfWrites: ToPG[Sesame, Authority] = defaultBindings.AuthorityBinding.authorityBinder

  override implicit def reads: Reads[AuthorityProtocol] = Authority.reads

  override implicit def writes: Writes[Authority] = Authority.writes

  override protected def fromInput(input: AuthorityProtocol, id: Option[UUID]): Authority = id match {
    case Some(uuid) => Authority(input.user, input.refRoles, uuid)
    case None => Authority(input.user, input.refRoles, Authority.randomUUID)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override protected def atomize(output: Authority): Try[Option[JsValue]] = {
    // TODO unify somehow
    import defaultBindings.EmployeeBinding.employeeBinder
    import defaultBindings.StudentBinding.studentBinder
    import defaultBindings.RefRoleBinding.refRoleBinder
    import Authority.atomicEmployeeWrites
    import Authority.atomicStudentWrites

    for {
      employee <- repository.get[Employee](Employee.generateUri(output.user)(namespace))
      student <- repository.get[Student](Student.generateUri(output.user)(namespace))
      refRoles <- repository.getMany[RefRole](output.refRoles.map(id => RefRole.generateUri(id)(namespace)))
    } yield {
      (employee, student) match {
        case (Some(e), None) => Some(Json.toJson(AuthorityEmployeeAtom(e, refRoles, output.id)))
        case (None, Some(s)) => Some(Json.toJson(AuthorityStudentAtom(s, refRoles, output.id)))
        case _ => None
      }
    }
  }

  override protected def atomizeMany(output: Set[Authority]): Try[JsValue] = Success(Json.toJson(output))

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Update => PartialSecureBlock(authority.update)
    case GetAll => PartialSecureBlock(authority.getAll)
    case Get => PartialSecureBlock(authority.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def compareModel(input: AuthorityProtocol, output: Authority): Boolean = input.refRoles == output.refRoles

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Authority]): Try[Set[Authority]] = Success(all)
}
