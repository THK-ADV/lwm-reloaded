package controllers

import java.util.UUID

import controllers.core.AbstractCRUDController
import dao.{AuthorityDao, RoleDao}
import database.{RoleDb, RoleTable}
import javax.inject.{Inject, Singleton}
import models.Role.{CourseManager, God}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object RoleController {
  lazy val labelAttribute = "label"
  lazy val filterAttribute = "filter"
  lazy val selectValue = "courseSensitive"
}

@Singleton
final class RoleController @Inject()(
  cc: ControllerComponents,
  val abstractDao: RoleDao,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[Role, RoleTable, RoleDb, Role](cc) {

  import controllers.core.DBFilterOps._

  override protected implicit val writes: Writes[Role] = Role.writes

  override protected implicit val reads: Reads[Role] = Role.reads

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import RoleController._

    (attribute, value) match {
      case (`labelAttribute`, l) =>
        l.makeLabelEqualsFilter
      case (`filterAttribute`, `selectValue`) =>
        Success(RoleDao.courseSensitiveFilter)
      case (`filterAttribute`, other) =>
        Failure(new Throwable(s"Value of [$filterAttribute] must be [$selectValue], but was $other"))
      case _ =>
        Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: Role, existingId: Option[UUID]): RoleDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(CourseManager))
    case GetAll => PartialSecureBlock(List(CourseManager))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()
}
