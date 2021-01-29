package controllers

import java.util.UUID

import dao._
import database.{AuthorityDb, AuthorityTable}
import javax.inject.{Inject, Singleton}
import models.Role._
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import security.SecurityActionChain

import scala.util.{Failure, Success, Try}

object AuthorityController {
  lazy val userAttribute = "user"
  lazy val courseAttribute = "course"
  lazy val roleAttribute = "role"
  lazy val roleLabelAttribute = "roleLabel"
  lazy val systemIdAttribute = "systemId"
}

@Singleton
final class AuthorityController @Inject()(cc: ControllerComponents, val abstractDao: AuthorityDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[AuthorityProtocol, AuthorityTable, AuthorityDb, AuthorityLike](cc) {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[AuthorityLike] = AuthorityLike.writes

  override protected implicit val reads: Reads[AuthorityProtocol] = AuthorityProtocol.reads

  override implicit val authorityDao: AuthorityDao = abstractDao

  override def invalidate(id: String, secureContext: SecureContext): Action[AnyContent] = contextFrom(Delete) asyncAction { _ =>
    (for {
      uuid <- id.uuidF
      deleted <- abstractDao.deleteAuthorityIfNotBasic(uuid)
    } yield deleted.toUniqueEntity).jsonResult
  }

  // TODO marcel:
  // TODO None & Employee
  // TODO Some(DB1) & CourseManager
  // TODO Some(DB2) & CourseManager
  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(List(CourseManager))
    case Delete => PartialSecureBlock(List(CourseManager))
    case GetAll => PartialSecureBlock(List(CourseManager, StudentRole, EmployeeRole))
    case Get => PartialSecureBlock(List(CourseManager))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import controllers.AuthorityController._
    import dao.AuthorityDao._
    import dao.helper.TableFilter.{systemIdFilter, userFilter}

    (attribute, value) match {
      case (`userAttribute`, user) => user.uuid map userFilter
      case (`courseAttribute`, course) => course.uuid map courseFilter
      case (`roleAttribute`, role) => role.uuid map roleFilter
      case (`roleLabelAttribute`, roleLabel) => Success(roleLabelFilter(roleLabel))
      case (`systemIdAttribute`, systemId) => Success(systemIdFilter(systemId))
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: AuthorityProtocol, existingId: Option[UUID]): AuthorityDb = AuthorityDb(
    protocol.user, protocol.role, protocol.course, id = existingId.getOrElse(UUID.randomUUID)
  )

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()
}