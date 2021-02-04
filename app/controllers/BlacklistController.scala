package controllers

import controllers.BlacklistController.BlacklistRangeCreationRequest
import controllers.helper.TimeRangeTableFilter
import dao._
import database.{BlacklistDb, BlacklistTable}
import models.{Blacklist, BlacklistProtocol}
import org.joda.time.LocalDate
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.ControllerComponents
import security.LWMRole.{Admin, EmployeeRole}
import security.SecurityActionChain
import service.BlacklistApiService

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import scala.util.Try

object BlacklistController {
  lazy val globalAttribute = "global"
  lazy val labelAttribute = "label"

  case class BlacklistRangeCreationRequest(label: String, start: LocalDate, end: LocalDate)

}

@Singleton
final class BlacklistController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: BlacklistDao, val blacklistService: BlacklistApiService, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[BlacklistProtocol, BlacklistTable, BlacklistDb, Blacklist](cc)
    with TimeRangeTableFilter[BlacklistTable] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[Blacklist] = Blacklist.writes

  override protected implicit val reads: Reads[BlacklistProtocol] = BlacklistProtocol.reads

  def createFor(year: String) = contextFrom(Create) asyncAction { implicit request =>
    import utils.Ops.unwrapTrys

    (for {
      blacklists <- fetch(year)
      partialCreated <- abstractDao.createManyPartial(blacklists)
      (succeeded, failed) = unwrapTrys(partialCreated)
    } yield (blacklists.map(_.toUniqueEntity), succeeded.map(_.toUniqueEntity), failed)).jsonResult
  }

  def preview(year: String) = contextFrom(Create) asyncAction { _ =>
    fetch(year).map(_.map(_.toUniqueEntity)).jsonResult
  }

  def createFromRange() = contextFrom(Create) asyncAction { request =>
    import service.BlacklistService.fromRange
    import utils.date.DateTimeJsonFormatter.readLocalDate
    implicit val blrReads: Reads[BlacklistRangeCreationRequest] = Json.reads[BlacklistRangeCreationRequest]

    (for {
      json <- Future.fromTry(parseJson(request)(blrReads))
      blacklists = fromRange(json.label, json.start, json.end)
      created <- abstractDao.createMany(blacklists)
    } yield created.map(_.toUniqueEntity)).jsonResult
  }

  private def fetch(year: String) = for {
    year <- Future.fromTry(Try(year.toInt))
    blacklists <- blacklistService.fetchLegalHolidays(year)
  } yield blacklists

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import BlacklistController._
    import dao.BlacklistDao._

    (attribute, value) match {
      case (`globalAttribute`, g) => g.boolean map globalFilter
      case (`labelAttribute`, l) => l.makeLabelEqualsFilter
      case _ => makeTimeRangeFilter(attribute, value)
    }
  }

  override protected def toDbModel(protocol: BlacklistProtocol, existingId: Option[UUID]): BlacklistDb = {
    import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter}
    BlacklistDb(protocol.label, protocol.date.sqlDate, protocol.start.sqlTime, protocol.end.sqlTime, protocol.global, id = existingId.getOrElse(UUID.randomUUID))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(EmployeeRole))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()
}
