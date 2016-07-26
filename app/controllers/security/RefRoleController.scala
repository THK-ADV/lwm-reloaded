package controllers.security

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.{RefRole, RefRoleAtom, RefRoleProtocol}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._
import store.bind.Descriptor.Descriptor
import scala.collection.Map
import scala.util.{Failure, Try}

object RefRoleController {
  val courseAttribute = "course"
}

class RefRoleController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RefRoleProtocol, RefRole, RefRoleAtom]{

  override implicit val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  override implicit val descriptor: Descriptor[Sesame, RefRole] = defaultBindings.RefRoleDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, RefRoleAtom] = defaultBindings.RefRoleAtomDescriptor

  override implicit val reads: Reads[RefRoleProtocol] = RefRole.reads

  override implicit val writes: Writes[RefRole] = RefRole.writes

  override implicit val writesAtom: Writes[RefRoleAtom] = RefRole.writesAtom

  override implicit val uriGenerator: UriGenerator[RefRole] = RefRole

  override protected def coatomic(atom: RefRoleAtom): RefRole = RefRole(atom.course map (_.id), atom.role.id, atom.invalidated, atom.id)

  override protected def compareModel(input: RefRoleProtocol, output: RefRole): Boolean = input.role == output.role

  override protected def fromInput(input: RefRoleProtocol, existing: Option[RefRole]): RefRole = existing match {
    case Some(refRole) => RefRole(input.course, input.role, refRole.invalidated, refRole.id)
    case None => RefRole(input.course, input.role)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    /*case Get => PartialSecureBlock(refRole.get)
    case GetAll => PartialSecureBlock(refRole.getAll)
    case _ => PartialSecureBlock(god)*/
    case _ => NonSecureBlock
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[RefRole]): Try[Set[RefRole]] = {
    import RefRoleController._

    queryString.foldRight(Try[Set[RefRole]](all)) {
      case ((`courseAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(id => set.filter(_.course.contains(id))))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }
}
