package controllers.crud.semester

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions._
import models.semester.{Blacklist, BlacklistProtocol}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import scala.collection.Map
import scala.util.{Success, Try}

class BlacklistCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[BlacklistProtocol, Blacklist, Blacklist] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.blacklistV1Json

  override implicit val descriptor: Descriptor[Sesame, Blacklist] = defaultBindings.BlacklistDescriptor

  override val descriptorAtom: Descriptor[Sesame, Blacklist] = descriptor

  override implicit val reads: Reads[BlacklistProtocol] = Blacklist.reads

  override implicit val writes: Writes[Blacklist] = Blacklist.writes

  override implicit val writesAtom: Writes[Blacklist] = Blacklist.writesAtom

  override implicit val uriGenerator: UriGenerator[Blacklist] = Blacklist

  override protected def coatomic(atom: Blacklist): Blacklist = atom

  override protected def compareModel(input: BlacklistProtocol, output: Blacklist): Boolean = {
    input.label == output.label && input.dates == output.dates
  }

  override protected def fromInput(input: BlacklistProtocol, existing: Option[Blacklist]): Blacklist = existing match {
    case Some(blacklist) => Blacklist(input.label, input.dates, blacklist.id)
    case None => Blacklist(input.label, input.dates, Blacklist.randomUUID)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(blacklist.get)
    case _ => PartialSecureBlock(prime)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Blacklist]): Try[Set[Blacklist]] = Success(all)

}
