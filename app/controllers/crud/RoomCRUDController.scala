package controllers.crud

import java.util.UUID

import models.{Room, RoomProtocol, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.Prefixes.LWMPrefix
import store.sparql.select._
import store.sparql.{select, Clause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

class RoomCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RoomProtocol, Room] {
   override implicit def rdfWrites: ToPG[Sesame, Room] = defaultBindings.RoomBinding.roomBinder

   override implicit def rdfReads: FromPG[Sesame, Room] = defaultBindings.RoomBinding.roomBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, Room] = defaultBindings.RoomBinding.classUri

   override implicit def uriGenerator: UriGenerator[Room] = Room

   override implicit def reads: Reads[RoomProtocol] = Room.reads

   override implicit def writes: Writes[Room] = Room.writes

   override protected def fromInput(input: RoomProtocol, id: Option[UUID]): Room = id match {
      case Some(uuid) => Room(input.label, input.description, uuid)
      case None => Room(input.label, input.description, Room.randomUUID)
   }

   override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Room]): Result = ???

   override val mimeType: LwmMimeType = LwmMimeType.roomV1Json


   override protected def compareModel(input: RoomProtocol, output: Room): Boolean = input.label == output.label

   override protected def existsQuery(input: RoomProtocol): (Clause, select.Var) = {
      lazy val prefixes = LWMPrefix[repository.Rdf]
      lazy val rdf = RDFPrefix[repository.Rdf]

      (select ("id") where {
         ^(v("s"), p(rdf.`type`), s(prefixes.Room)) .
           ^(v("s"), p(prefixes.label), o(input.label)).
           ^(v("s"), p(prefixes.id), v("id"))
      }, v("id"))
   }
}
