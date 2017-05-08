package controllers

import models.Permissions._
import models.{SesameRoom, SesameRoomProtocol, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, RoleServiceLike, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.select._
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class RoomCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleServiceLike) extends AbstractCRUDController[SesameRoomProtocol, SesameRoom, SesameRoom] {

  override val mimeType: LwmMimeType = LwmMimeType.roomV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameRoom] = defaultBindings.RoomDescriptor

  override val descriptorAtom: Descriptor[Sesame, SesameRoom] = descriptor

  override implicit val reads: Reads[SesameRoomProtocol] = SesameRoom.reads

  override implicit val writes: Writes[SesameRoom] = SesameRoom.writes

  override  val writesAtom: Writes[SesameRoom] = SesameRoom.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameRoom] = SesameRoom

  override protected def coAtomic(atom: SesameRoom): SesameRoom = atom

  override protected def compareModel(input: SesameRoomProtocol, output: SesameRoom): Boolean = input.description == output.description

  override protected def fromInput(input: SesameRoomProtocol, existing: Option[SesameRoom]): SesameRoom = existing match {
    case Some(room) => SesameRoom(input.label, input.description, room.invalidated, room.id)
    case None => SesameRoom(input.label, input.description)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(room.get)
    case GetAll => PartialSecureBlock(room.getAll)
    case _ => PartialSecureBlock(prime)
  }

  override protected def existsQuery(input: SesameRoomProtocol): Clause = {
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    select("id") where {
      **(v("s"), p(rdf.`type`), s(lwm.Room)).
        **(v("s"), p(lwm.label), o(input.label))
    }
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameRoom]): Try[Set[SesameRoom]] = Success(all)

}
