package controllers

import java.util.UUID

import models.Permissions.{prime, room}
import models.{PostgresRoomProtocol, RoomDb}
import play.api.mvc.Controller
import services._
import store.{RoomTable, TableFilter}
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object RoomControllerPostgres {
  lazy val labelAttribute = "label"
}

final class RoomControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleService, val roomService: RoomService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with ContentTyped
  with Chunked
  with PostgresResult {

  override implicit def mimeType = LwmMimeType.roomV1Json
  import scala.concurrent.ExecutionContext.Implicits.global
  import models.PostgresRoom.{writes, reads}


  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(room.get)
    case GetAll => PartialSecureBlock(room.getAll)
    case _ => PartialSecureBlock(prime)
  }

  def create = contextFrom(Create) asyncContentTypedAction { request =>
    (for {
      protocol <- Future.fromTry(parse[PostgresRoomProtocol](request))
      roomDb = RoomDb(protocol.label, protocol.description)
      room <- roomService.create(roomDb)
    } yield room.toRoom).jsonResult
  }

  def update (id: String) = contextFrom(Update) asyncContentTypedAction { request =>
    val uuid = UUID.fromString(id)

    (for {
      protocol <- Future.fromTry(parse[PostgresRoomProtocol](request))
      roomDb = RoomDb(protocol.label, protocol.description, None, uuid)
      room <- roomService.update(roomDb)
    } yield room.map(_.toRoom)).jsonResult(uuid)
  }

  def all = contextFrom(GetAll) asyncAction { request =>
    import RoomControllerPostgres._

    val roomFilter = request.queryString.foldLeft(Try(List.empty[TableFilter[RoomTable]])){
      case (list, (`labelAttribute`, label)) => list.map(_.+:(RoomLabelFilter(label.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
    (for{
      list <- Future.fromTry(roomFilter)
      rooms <- roomService.get(list)
    } yield rooms).jsonResult
  }

  def get(id: String) = contextFrom(Get) asyncAction { _ =>
    roomService.get(List(RoomIdFilter(id))).map(_.headOption).jsonResult(id)
  }

  def delete(id: String) = contextFrom(Delete) asyncAction { _ =>
    val uuid = UUID.fromString(id)

    roomService.delete(uuid).map(_.map(_.toRoom)).jsonResult(uuid)
  }
}