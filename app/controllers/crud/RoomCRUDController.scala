package controllers.crud

import models.{Room, RoomProtocol, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.sparql.select._
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._
import store.bind.Descriptor.Descriptor
import scala.collection.Map
import scala.util.{Success, Try}

class RoomCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RoomProtocol, Room, Room] {

  override val mimeType: LwmMimeType = LwmMimeType.roomV1Json

  override implicit val descriptor: Descriptor[Sesame, Room] = defaultBindings.RoomDescriptor

  override val descriptorAtom: Descriptor[Sesame, Room] = descriptor

  override implicit val reads: Reads[RoomProtocol] = Room.reads

  override implicit val writes: Writes[Room] = Room.writes

  override  val writesAtom: Writes[Room] = Room.writesAtom

  override implicit val uriGenerator: UriGenerator[Room] = Room

  override protected def coAtomic(atom: Room): Room = atom

  override protected def compareModel(input: RoomProtocol, output: Room): Boolean = input.description == output.description

  override protected def fromInput(input: RoomProtocol, existing: Option[Room]): Room = existing match {
    case Some(room) => Room(input.label, input.description, room.invalidated, room.id)
    case None => Room(input.label, input.description)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(room.get)
    case GetAll => PartialSecureBlock(room.getAll)
    case _ => PartialSecureBlock(prime)
  }

  override protected def existsQuery(input: RoomProtocol): (Clause, select.Var) = {
    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    (select("id") where {
      **(v("s"), p(rdf.`type`), s(prefixes.Room)).
        **(v("s"), p(prefixes.label), o(input.label)).
        **(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Room]): Try[Set[Room]] = Success(all)

}
