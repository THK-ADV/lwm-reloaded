package controllers

import java.util.UUID

import controllers.helper.GroupingStrategyAttributeFilter
import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{CourseEmployee, CourseManager, God}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import services._
import database.{GroupDb, GroupTable, TableFilter}
import security.SecurityActionChain

import scala.concurrent.Future
import scala.util.{Failure, Try}

object GroupController {
  lazy val labworkAttribute = "labwork"
  lazy val studentAttribute = "student"
  lazy val labelAttribute = "label"
}

@Singleton
final class GroupController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: GroupDao, val labworkApplicationDao: LabworkApplicationDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[GroupProtocol, GroupTable, GroupDb, GroupLike](cc)
    with GroupingStrategyAttributeFilter {

  import controllers.GroupController._

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[GroupLike] = GroupLike.writes

  override protected implicit val reads: Reads[GroupProtocol] = GroupProtocol.reads

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(labworkAttribute -> Seq(labwork)))
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { request =>
    (for {
      applications <- labworkApplicationDao.get(List(LabworkApplicationLabworkFilter(labwork)), atomic = false)
      apps = applications.map(_.asInstanceOf[LabworkApplication]).toVector
      groupingStrategy <- Future.fromTry(extractGroupingStrategy(request.queryString))
      groups = GroupService.groupApplicantsBy(groupingStrategy, apps, UUID.fromString(labwork))
    } yield groups).jsonResult
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[GroupTable]]]): Try[List[TableFilter[GroupTable]]] = {
    (appendTo, (attribute, value)) match {
      case (list, (`studentAttribute`, student)) => list.map(_.+:(GroupStudentTableFilter(student)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(GroupLabworkTableFilter(labwork)))
      case (list, (`labelAttribute`, label)) => list.map(_.+:(GroupLabelTableFilter(label)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: GroupProtocol, existingId: Option[UUID]): GroupDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbidden()
}
