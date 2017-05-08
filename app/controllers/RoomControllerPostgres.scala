package controllers

import java.util.UUID

import models.Permissions.{prime, room}
import models.{PostgresRoom, PostgresRoomProtocol, RoomDb}
import play.api.libs.json.{Reads, Writes}
import services._
import store.{RoomTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object RoomControllerPostgres {
  lazy val labelAttribute = "label"
}

final class RoomControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val roomService: RoomService) extends
  AbstractCRUDControllerPostgres[PostgresRoomProtocol, RoomTable, RoomDb, PostgresRoom]{

  override implicit def mimeType = LwmMimeType.roomV1Json


  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(room.get)
    case GetAll => PartialSecureBlock(room.getAll)
    case _ => PartialSecureBlock(prime)
  }

  override protected implicit def writes: Writes[PostgresRoom] = PostgresRoom.writes

  override protected implicit def reads: Reads[PostgresRoomProtocol] = PostgresRoom.reads

  override protected def abstractDao: AbstractDao[RoomTable, RoomDb, PostgresRoom] = roomService

  override protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[RoomTable]]]): Try[List[TableFilter[RoomTable]]] = {
    import controllers.RoomControllerPostgres._


    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(RoomLabelFilter(label.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresRoomProtocol, existingId: Option[UUID]): RoomDb = RoomDb.from(protocol, existingId)

  override protected def toLwmModel(dbModel: RoomDb): PostgresRoom = dbModel.toRoom
}