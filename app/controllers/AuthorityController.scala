package controllers

import dao._
import database.{AuthorityDb, AuthorityTable}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import security.LWMRole._
import security.SecurityActionChain

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object AuthorityController {
  lazy val userAttribute = "user"
  lazy val courseAttribute = "course"
  lazy val roleAttribute = "role"
  lazy val roleLabelAttribute = "roleLabel"
  lazy val systemIdAttribute = "systemId"
}

@Singleton
final class AuthorityController @Inject()(
  cc: ControllerComponents,
  val abstractDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[AuthorityProtocol, AuthorityTable, AuthorityDb, AuthorityLike](cc) {

  import controllers.AuthorityController._
  import dao.AuthorityDao._
  import dao.helper.TableFilter.{systemIdFilter, userFilter}

  override protected implicit val writes: Writes[AuthorityLike] = AuthorityLike.writes

  override protected implicit val reads: Reads[AuthorityProtocol] = AuthorityProtocol.reads

  override implicit val authorityDao: AuthorityDao = abstractDao

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    create(NonSecureBlock)(request)
  }

  def invalidateFrom(course: String, id: String): Action[AnyContent] = restrictedContext(course)(Delete) asyncAction { _ =>
    (for {
      uuid <- id.uuidF
      deleted <- abstractDao.deleteAuthorityIfNotBasic(uuid)
    } yield deleted.toUniqueEntity).jsonResult
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] =
    (attribute, value) match {
      case (`userAttribute`, user) => user.uuid map userFilter
      case (`courseAttribute`, course) => course.uuid map courseFilter
      case (`roleAttribute`, role) => role.uuid map roleFilter
      case (`roleLabelAttribute`, roleLabel) => Success(roleLabelFilter(roleLabel))
      case (`systemIdAttribute`, systemId) => Success(systemIdFilter(systemId))
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }

  override protected def toDbModel(protocol: AuthorityProtocol, existingId: Option[UUID]): AuthorityDb =
    AuthorityDb(
      protocol.user,
      protocol.role,
      protocol.course,
      id = existingId.getOrElse(UUID.randomUUID)
    )

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create | Delete => SecureBlock(restrictionId, List(CourseManager))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case GetAll | Get => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case _ => PartialSecureBlock(List(God))
  }
}