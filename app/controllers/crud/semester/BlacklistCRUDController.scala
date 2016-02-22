package controllers.crud.semester

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions._
import models.semester.{Blacklist, BlacklistProtocol}
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

class BlacklistCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[BlacklistProtocol, Blacklist] {

  override implicit def reads: Reads[BlacklistProtocol] = Blacklist.reads

  override implicit def writes: Writes[Blacklist] = Blacklist.writes

  override implicit def rdfReads: FromPG[Sesame, Blacklist] = defaultBindings.BlacklistBinding.blacklistBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Blacklist] = defaultBindings.BlacklistBinding.classUri

  override implicit def uriGenerator: UriGenerator[Blacklist] = Blacklist

  override implicit def rdfWrites: ToPG[Sesame, Blacklist] = defaultBindings.BlacklistBinding.blacklistBinder

  override implicit val mimeType: LwmMimeType = LwmMimeType.blacklistV1Json

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Blacklist]): Result = ???

  override protected def fromInput(input: BlacklistProtocol, id: Option[UUID]): Blacklist = id match {
    case Some(uuid) => Blacklist(input.dates, uuid)
    case None => Blacklist(input.dates, Blacklist.randomUUID)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Set(prime))
  }

  override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Set(prime))
  }
}
