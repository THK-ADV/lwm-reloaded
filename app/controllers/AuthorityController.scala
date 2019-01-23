package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role._
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import database.{AuthorityDb, AuthorityTable, TableFilter}
import utils.SecuredAction

import scala.concurrent.Future
import scala.util.{Failure, Try}

object AuthorityController {
  lazy val userAttribute = "user"
  lazy val courseAttribute = "course"
  lazy val roleAttribute = "role"
  lazy val roleLabelAttribute = "roleLabel"
}

@Singleton
final class AuthorityController @Inject()(cc: ControllerComponents, val abstractDao: AuthorityDao, val securedAction: SecuredAction)
  extends AbstractCRUDController[AuthorityProtocol, AuthorityTable, AuthorityDb, AuthorityLike](cc) {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[AuthorityLike] = AuthorityLike.writes

  override protected implicit val reads: Reads[AuthorityProtocol] = AuthorityProtocol.reads

  override implicit val authorityDao: AuthorityDao = abstractDao

  override def delete(id: String, secureContext: SecureContext): Action[AnyContent] = contextFrom(Delete) asyncAction { _ =>
    val uuid = UUID.fromString(id)

    for {
      auth <- abstractDao.getById(id)
      student = Role.StudentRole.label
      employee = Role.EmployeeRole.label
      isBasicRole = auth.map(_.asInstanceOf[AuthorityAtom]).exists(a => a.role.label == student || a.role.label == employee)
      result <- if (!isBasicRole)
        delete0(uuid)
      else
        Future.successful(preconditionFailed(s"The user associated with $id have to remain with at least one basic role, namely $student or $employee"))
    } yield result
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(List(RightsManager))
    case Delete => PartialSecureBlock(List(RightsManager))
    case GetAll => PartialSecureBlock(List(RightsManager, StudentRole, EmployeeRole))
    case Get => PartialSecureBlock(List(RightsManager))
    case _ => PartialSecureBlock(List(God))
  }


  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[AuthorityTable]]]): Try[List[TableFilter[AuthorityTable]]] = {
    import controllers.AuthorityController._

    (appendTo, (attribute, value)) match {
      case (list, (`userAttribute`, user)) => list.map(_.+:(AuthorityUserFilter(user)))
      case (list, (`courseAttribute`, course)) => list.map(_.+:(AuthorityCourseFilter(course)))
      case (list, (`roleAttribute`, role)) => list.map(_.+:(AuthorityRoleFilter(role)))
      case (list, (`roleLabelAttribute`, role)) => list.map(_.+:(AuthorityRoleLabelFilter(role)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: AuthorityProtocol, existingId: Option[UUID]): AuthorityDb = AuthorityDb(
    protocol.user, protocol.role, protocol.course, id = existingId.getOrElse(UUID.randomUUID)
  )

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
