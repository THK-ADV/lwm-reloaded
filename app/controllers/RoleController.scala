package controllers

import models.Permissions._
import models.{SesameRole, SesameRoleProtocol, UriGenerator}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class RoleController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[SesameRoleProtocol, SesameRole, SesameRole] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.roleV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameRole] = defaultBindings.RoleDescriptor

  override val descriptorAtom: Descriptor[Sesame, SesameRole] = descriptor

  override implicit val reads: Reads[SesameRoleProtocol] = SesameRole.reads

  override implicit val writes: Writes[SesameRole] = SesameRole.writes

  override implicit val writesAtom: Writes[SesameRole] = SesameRole.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameRole] = SesameRole

  override protected def coAtomic(atom: SesameRole): SesameRole = atom

  override protected def compareModel(input: SesameRoleProtocol, output: SesameRole): Boolean = input.permissions == output.permissions

  override protected def fromInput(input: SesameRoleProtocol, existing: Option[SesameRole]): SesameRole = existing match {
    case Some(role) => SesameRole(input.label, input.permissions, role.invalidated, role.id)
    case None => SesameRole(input.label, input.permissions)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(role.get)
    case GetAll => PartialSecureBlock(role.getAll)
    case Update => PartialSecureBlock(prime)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameRole]): Try[Set[SesameRole]] = Success(all)
}
