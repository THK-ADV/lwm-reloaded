package controllers.security

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.{Course, UriGenerator}
import models.security.{RefRole, RefRoleAtom, RefRoleProtocol, Role}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object RefRoleController {
  val courseAttribute = "course"
}

class RefRoleController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RefRoleProtocol, RefRole]{

  override implicit def reads: Reads[RefRoleProtocol] = RefRole.reads

  override implicit def writes: Writes[RefRole] = RefRole.writes

  override implicit def rdfReads: FromPG[Sesame, RefRole] = defaultBindings.RefRoleBinding.refRoleBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, RefRole] = defaultBindings.RefRoleBinding.classUri

  override implicit def uriGenerator: UriGenerator[RefRole] = RefRole

  override implicit def rdfWrites: ToPG[Sesame, RefRole] = defaultBindings.RefRoleBinding.refRoleBinder

  override protected def fromInput(input: RefRoleProtocol, existing: Option[RefRole]): RefRole = existing match {
    case Some(refRole) => RefRole(input.course, input.role, refRole.id)
    case None => RefRole(input.course, input.role, RefRole.randomUUID)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  override protected def compareModel(input: RefRoleProtocol, output: RefRole): Boolean = input.role == output.role

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[RefRole]): Try[Set[RefRole]] = {
    import RefRoleController._

    queryString.foldRight(Try[Set[RefRole]](all)) {
      case ((`courseAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(id => set.filter(_.course.contains(id))))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def atomize(output: RefRole): Try[Option[JsValue]] = {
    import defaultBindings.RoleBinding.roleBinder
    import defaultBindings.CourseBinding.courseBinder
    import RefRole.atomicWrites

    for {
      role <- repository.get[Role](Role.generateUri(output.role)(namespace))
      course <- output.course.fold[Try[Option[Course]]](Success(None))(id => repository.get[Course](Course.generateUri(id)(namespace)))
    } yield for {
        r <- role
      } yield {
        val atom = RefRoleAtom(course, r, output.id)
        Json.toJson(atom)
      }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(refRole.get)
    case GetAll => PartialSecureBlock(refRole.getAll)
    case _ => PartialSecureBlock(god)
  }
}
