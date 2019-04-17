package controllers

import java.util.UUID

import dao._
import database.{AssignmentPlanDb, AssignmentPlanTable}
import javax.inject.{Inject, Singleton}
import models._
import org.joda.time.DateTime
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.util.{Failure, Try}

object AssignmentPlanController {
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
}

@Singleton
final class AssignmentPlanController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: AssignmentPlanDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[AssignmentPlanProtocol, AssignmentPlanTable, AssignmentPlanDb, AssignmentPlanLike](cc) {

  override protected implicit val writes: Writes[AssignmentPlanLike] = AssignmentPlanLike.writes

  override protected implicit val reads: Reads[AssignmentPlanProtocol] = AssignmentPlanProtocol.reads

  override protected def toDbModel(protocol: AssignmentPlanProtocol, existingId: Option[UUID]): AssignmentPlanDb = {
    import utils.date.DateTimeOps.DateTimeConverter
    AssignmentPlanDb(protocol.labwork, protocol.attendance, protocol.mandatory, protocol.entries, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }

  import models.Role._

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def deleteFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    delete(id, NonSecureBlock)(request)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    import controllers.AssignmentPlanController.courseAttribute

    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import AssignmentPlanController._

    (attribute, value) match {
      case (`courseAttribute`, course) => course.makeCourseFilter
      case (`labworkAttribute`, labwork) => labwork.makeLabworkFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }
}
