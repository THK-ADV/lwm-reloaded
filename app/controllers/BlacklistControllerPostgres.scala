package controllers

import java.util.UUID

import dao._
import models.Permissions.{blacklist, prime}
import models.{BlacklistDb, PostgresBlacklist, PostgresBlacklistProtocol}
import play.api.libs.json.{Reads, Writes}
import services._
import store.{BlacklistTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object BlacklistControllerPostgres {
  lazy val globalAttribute = "global"
  lazy val labelAttribute = "label"
  lazy val dateAttribute = "date"
  lazy val startAttribute = "start"
  lazy val endAttribute = "end"
  lazy val sinceAttribute = "since"
  lazy val untilAttribute = "until"
}

final class BlacklistControllerPostgres(val roleService: RoleServiceLike, val sessionService: SessionHandlingService, val abstractDao: BlacklistDao, val blacklistService: BlacklistServiceLike)
  extends AbstractCRUDControllerPostgres[PostgresBlacklistProtocol, BlacklistTable, BlacklistDb, PostgresBlacklist] {

  override protected implicit val writes: Writes[PostgresBlacklist] = PostgresBlacklist.writes

  override protected implicit val reads: Reads[PostgresBlacklistProtocol] = PostgresBlacklist.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[BlacklistTable]]]): Try[List[TableFilter[BlacklistTable]]] = {
    import controllers.BlacklistControllerPostgres._

    (appendTo, (attribute, value)) match {
      case (list, (`globalAttribute`, global)) => list.map(_.+:(BlacklistGlobalFilter(global)))
      case (list, (`labelAttribute`, label)) => list.map(_.+:(BlacklistLabelFilter(label)))
      case (list, (`dateAttribute`, date)) => list.map(_.+:(BlacklistDateFilter(date)))
      case (list, (`startAttribute`, start)) => list.map(_.+:(BlacklistStartFilter(start)))
      case (list, (`endAttribute`, end)) => list.map(_.+:(BlacklistEndFilter(end)))
      case (list, (`sinceAttribute`, since)) => list.map(_.+:(BlacklistSinceFilter(since)))
      case (list, (`untilAttribute`, until)) => list.map(_.+:(BlacklistUntilFilter(until)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresBlacklistProtocol, existingId: Option[UUID]): BlacklistDb = BlacklistDb.from(protocol, existingId)

  override implicit val mimeType = LwmMimeType.blacklistV1Json

  def createFor(year: String) = contextFrom(Create) asyncContentTypedAction { implicit request =>
    import scala.concurrent.ExecutionContext.Implicits.global

    (for {
      blacklists <- blacklistService.fetchByYear2(year)
      created <- abstractDao.createMany(blacklists)
    } yield created.map(_.toLwmModel)).jsonResult
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(blacklist.get)
    case _ => PartialSecureBlock(prime)
  }
}
