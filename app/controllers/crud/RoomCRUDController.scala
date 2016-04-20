package controllers.crud

import models.{Room, RoomProtocol, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.sparql.select._
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._

import scala.collection.Map
import scala.util.{Success, Try}

class RoomCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RoomProtocol, Room] {
   override implicit def rdfWrites: ToPG[Sesame, Room] = defaultBindings.RoomBinding.roomBinder

   override implicit def rdfReads: FromPG[Sesame, Room] = defaultBindings.RoomBinding.roomBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, Room] = defaultBindings.RoomBinding.classUri

   override implicit def uriGenerator: UriGenerator[Room] = Room

   override implicit def reads: Reads[RoomProtocol] = Room.reads

   override implicit def writes: Writes[Room] = Room.writes

   override protected def fromInput(input: RoomProtocol, existing: Option[Room]): Room = existing match {
      case Some(room) => Room(input.label, input.description, room.id)
      case None => Room(input.label, input.description, Room.randomUUID)
   }

   override val mimeType: LwmMimeType = LwmMimeType.roomV1Json

   override protected def compareModel(input: RoomProtocol, output: Room): Boolean = input.description == output.description

   override protected def existsQuery(input: RoomProtocol): (Clause, select.Var) = {
      lazy val prefixes = LWMPrefix[repository.Rdf]
      lazy val rdf = RDFPrefix[repository.Rdf]

      (select ("id") where {
         **(v("s"), p(rdf.`type`), s(prefixes.Room)) .
           **(v("s"), p(prefixes.label), o(input.label)).
           **(v("s"), p(prefixes.id), v("id"))
      }, v("id"))
   }

   override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Room]): Try[Set[Room]] = Success(all)

   override protected def atomize(output: Room): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

   override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case Get => PartialSecureBlock(room.get)
      case GetAll => PartialSecureBlock(room.getAll)
      case _ => PartialSecureBlock(prime)
   }
}
