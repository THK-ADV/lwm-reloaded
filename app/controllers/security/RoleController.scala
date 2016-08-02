package controllers.security

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.{Role, RoleProtocol}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import models.security.Permissions._
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import scala.collection.Map
import scala.util.{Success, Try}

class RoleController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RoleProtocol, Role, Role] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.roleV1Json

  override implicit val descriptor: Descriptor[Sesame, Role] = defaultBindings.RoleDescriptor

  override val descriptorAtom: Descriptor[Sesame, Role] = descriptor

  override implicit val reads: Reads[RoleProtocol] = Role.reads

  override implicit val writes: Writes[Role] = Role.writes

  override implicit val writesAtom: Writes[Role] = Role.writesAtom

  override implicit val uriGenerator: UriGenerator[Role] = Role

  override protected def coAtomic(atom: Role): Role = atom

  override protected def compareModel(input: RoleProtocol, output: Role): Boolean = input.permissions == output.permissions

  override protected def fromInput(input: RoleProtocol, existing: Option[Role]): Role = existing match {
    case Some(role) => Role(input.label, input.permissions, role.invalidated, role.id)
    case None => Role(input.label, input.permissions)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(role.get)
    case GetAll => PartialSecureBlock(role.getAll)
    case Update => PartialSecureBlock(prime)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Role]): Try[Set[Role]] = Success(all)
}
