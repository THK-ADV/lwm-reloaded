package controllers.crud

import java.util.UUID

import models.{RoomProtocol, Room, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import store.{Namespace, SesameRepository}

import scala.collection.Map

class RoomCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[RoomProtocol, Room] {
   override implicit def rdfWrites: ToPG[Sesame, Room] = defaultBindings.RoomBinding.roomBinder

   override implicit def rdfReads: FromPG[Sesame, Room] = defaultBindings.RoomBinding.roomBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, Room] = defaultBindings.RoomBinding.classUri

   override implicit def uriGenerator: UriGenerator[Room] = Room

   override implicit def reads: Reads[RoomProtocol] = Room.reads

   override implicit def writes: Writes[Room] = Room.writes

   override def getWithFilter(queryString: Map[String, Seq[String]]): Result = ???

   override protected def fromInput(input: RoomProtocol, id: Option[UUID]): Room = ???
}
