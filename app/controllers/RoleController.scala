package controllers

import dao.{AuthorityDao, RoleDao}
import database.{RoleDb, RoleTable}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.LWMRole.CourseManager
import security.SecurityActionChain

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.util.{Failure, Success, Try}

object RoleController {
  lazy val labelAttribute = "label"
  lazy val selectAttribute = "select"
  lazy val courseRelatedValue = "courseRelated"
}

@Singleton
final class RoleController @Inject()(
  cc: ControllerComponents,
  val abstractDao: RoleDao,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain
) extends AbstractCRUDController[Role, RoleTable, RoleDb, Role](cc) {

  import RoleController._

  override protected implicit val writes: Writes[Role] = Role.writes

  override protected implicit val reads: Reads[Role] = Role.reads

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] =
    (attribute, value) match {
      case (`labelAttribute`, l) => l.makeLabelEqualsFilter
      case (`selectAttribute`, `courseRelatedValue`) => Success(RoleDao.courseRelated)
      case (`selectAttribute`, other) =>
        Failure(new Throwable(s"Value of [$selectAttribute] must be [$courseRelatedValue], but was $other"))
      case _ =>
        Failure(new Throwable(s"Unknown attribute $attribute"))
    }

  override protected def toDbModel(protocol: Role, existingId: Option[UUID]): RoleDb = ???

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] =
    forbiddenAction()

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case GetAll => SecureBlock(restrictionId, List(CourseManager))
  }
}
