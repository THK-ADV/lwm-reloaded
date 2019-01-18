package controllers

import java.util.UUID

import dao.{AuthorityDao, RoomDao, RoomLabelFilter}
import models.Role.{Admin, Employee, Student}
import models.{PostgresRoom, PostgresRoomProtocol, RoomDb}
import play.api.libs.json.{Reads, Writes}
import store.{RoomTable, TableFilter}

import scala.util.{Failure, Try}

//object RoomControllerPostgres {
//  lazy val labelAttribute = "label"
//}
//
//final class RoomControllerPostgres(val authorityDao: AuthorityDao, val abstractDao: RoomDao)
//  extends AbstractCRUDControllerPostgres[PostgresRoomProtocol, RoomTable, RoomDb, PostgresRoom] {
//
//  override protected implicit val writes: Writes[PostgresRoom] = PostgresRoom.writes
//
//  override protected implicit val reads: Reads[PostgresRoomProtocol] = PostgresRoomProtocol.reads
//
//  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
//    case Get => PartialSecureBlock(List(Student, Employee))
//    case GetAll => PartialSecureBlock(List(Employee))
//    case _ => PartialSecureBlock(List(Admin))
//  }
//
//
//  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[RoomTable]]]): Try[List[TableFilter[RoomTable]]] = {
//    import controllers.RoomControllerPostgres._
//
//    (appendTo, (attribute, value)) match {
//      case (list, (`labelAttribute`, label)) => list.map(_.+:(RoomLabelFilter(label)))
//      case _ => Failure(new Throwable("Unknown attribute"))
//    }
//  }
//
//  override protected def toDbModel(protocol: PostgresRoomProtocol, existingId: Option[UUID]): RoomDb = RoomDb.from(protocol, existingId)
//}