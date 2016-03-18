package controllers.crud.semester

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions._
import models.semester.{Blacklist, BlacklistProtocol}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class BlacklistCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[BlacklistProtocol, Blacklist] {

  override implicit def reads: Reads[BlacklistProtocol] = Blacklist.reads

  override implicit def writes: Writes[Blacklist] = Blacklist.writes

  override implicit def rdfReads: FromPG[Sesame, Blacklist] = defaultBindings.BlacklistBinding.blacklistBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Blacklist] = defaultBindings.BlacklistBinding.classUri

  override implicit def uriGenerator: UriGenerator[Blacklist] = Blacklist

  override implicit def rdfWrites: ToPG[Sesame, Blacklist] = defaultBindings.BlacklistBinding.blacklistBinder

  override implicit val mimeType: LwmMimeType = LwmMimeType.blacklistV1Json

  override protected def fromInput(input: BlacklistProtocol, existing: Option[Blacklist]): Blacklist = existing match {
    case Some(blacklist) => Blacklist(input.label, input.dates, blacklist.id)
    case None => Blacklist(input.label, input.dates, Blacklist.randomUUID)
  }

  override protected def compareModel(input: BlacklistProtocol, output: Blacklist): Boolean = {
    input.label == output.label && input.dates == output.dates
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Blacklist]): Try[Set[Blacklist]] = Success(all)

  override protected def atomize(output: Blacklist): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(blacklist.get)
    case _ => PartialSecureBlock(prime)
  }
}
