package controllers.security

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.{Course, UriGenerator}
import models.security.{RefRoleAtom, Role, RefRole, RefRoleProtocol}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._
import scala.collection.Map
import scala.util.{Success, Try}


class RefRoleController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RefRoleProtocol, RefRole]{
  override implicit def reads: Reads[RefRoleProtocol] = RefRole.reads

  override implicit def writes: Writes[RefRole] = RefRole.writes

  override implicit def rdfReads: FromPG[Sesame, RefRole] = defaultBindings.RefRoleBinding.refRoleBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, RefRole] = defaultBindings.RefRoleBinding.classUri

  override implicit def uriGenerator: UriGenerator[RefRole] = RefRole

  override implicit def rdfWrites: ToPG[Sesame, RefRole] = defaultBindings.RefRoleBinding.refRoleBinder

  override protected def fromInput(input: RefRoleProtocol, existing: Option[RefRole]): RefRole = existing match {
    case Some(refRole) => RefRole(input.module, input.role, refRole.id)
    case None => RefRole(input.module, input.role, RefRole.randomUUID)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  override protected def compareModel(input: RefRoleProtocol, output: RefRole): Boolean = input.role == output.role

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[RefRole]): Try[Set[RefRole]] = Success(all)

  override protected def atomize(output: RefRole): Try[Option[JsValue]] = {
    import defaultBindings.RoleBinding.roleBinder
    import defaultBindings.CourseBinding.courseBinder
    import RefRole.atomicWrites

    for {
      role <- repository.get[Role](Role.generateUri(output.role)(namespace))
      course <- output.module.fold[Try[Option[Course]]](Success(None))(id => repository.get[Course](Course.generateUri(id)(namespace)))
    } yield {
      for  {
        r <- role
      } yield {
        val atom = RefRoleAtom(course, r, output.id)
        Json.toJson(atom)
      }
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(refRole.get)
    case GetAll => PartialSecureBlock(refRole.getAll)
    case _ => PartialSecureBlock(god)
  }
}
