package controllers

import java.util.UUID

import controllers.core.AbstractCRUDController
import dao.{AuthorityDao, RoomDao}
import database.{RoomDb, RoomTable}
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, StudentRole}
import models.{Room, RoomProtocol}
import org.joda.time.DateTime
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Try}

object RoomController {
  lazy val labelAttribute = "label"
}

@Singleton
final class RoomController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: RoomDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[RoomProtocol, RoomTable, RoomDb, Room](cc) {

  import controllers.core.DBFilterOps._

  override protected implicit val writes: Writes[Room] = Room.writes

  override protected implicit val reads: Reads[RoomProtocol] = RoomProtocol.reads

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case GetAll => PartialSecureBlock(List(EmployeeRole))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import RoomController._

    (attribute, value) match {
      case (`labelAttribute`, l) => l.makeLabelEqualsFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: RoomProtocol, existingId: Option[UUID]): RoomDb = {
    import utils.date.DateTimeOps.DateTimeConverter
    RoomDb(protocol.label, protocol.description, protocol.capacity, DateTime.now.timestamp, None, existingId getOrElse UUID.randomUUID)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()
}