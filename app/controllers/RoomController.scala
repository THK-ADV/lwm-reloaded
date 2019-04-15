package controllers

import java.util.UUID

import dao.{AuthorityDao, RoomDao}
import database.{RoomDb, RoomTable}
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, StudentRole}
import models.{Room, RoomProtocol}
import org.joda.time.DateTime
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

object RoomController {
  lazy val labelAttribute = "label"
}

@Singleton
final class RoomController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: RoomDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[RoomProtocol, RoomTable, RoomDb, Room](cc) {

  override protected implicit val writes: Writes[Room] = Room.writes

  override protected implicit val reads: Reads[RoomProtocol] = RoomProtocol.reads

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case GetAll => PartialSecureBlock(List(EmployeeRole))
    case _ => PartialSecureBlock(List(Admin))
  }


  //  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[RoomTable]]]): Try[List[TableFilter[RoomTable]]] = {
  //    import controllers.RoomController._
  //
  //    (appendTo, (attribute, value)) match {
  //      case (list, (`labelAttribute`, label)) => list.map(_.+:(RoomLabelFilter(label)))
  //      case _ => Failure(new Throwable("Unknown attribute"))
  //    }
  //  }

  override protected def toDbModel(protocol: RoomProtocol, existingId: Option[UUID]): RoomDb = {
    import utils.date.DateTimeOps.DateTimeConverter
    RoomDb(protocol.label, protocol.description, protocol.capacity, DateTime.now.timestamp, None, existingId getOrElse UUID.randomUUID)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}