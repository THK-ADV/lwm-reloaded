package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import database.{AssignmentPlanDb, AssignmentPlanTable, TableFilter}
import utils.SecuredAction

import scala.util.{Failure, Try}

object AssignmentPlanController {
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
}

@Singleton
final class AssignmentPlanController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: AssignmentPlanDao, val securedAction: SecuredAction)
  extends AbstractCRUDController[AssignmentPlanProtocol, AssignmentPlanTable, AssignmentPlanDb, AssignmentPlanLike](cc) {

  override protected implicit val writes: Writes[AssignmentPlanLike] = AssignmentPlanLike.writes

  override protected implicit val reads: Reads[AssignmentPlanProtocol] = AssignmentPlanProtocol.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[AssignmentPlanTable]]]): Try[List[TableFilter[AssignmentPlanTable]]] = {
    import controllers.AssignmentPlanController._

    (appendTo, (attribute, value)) match {
      case (list, (`courseAttribute`, course)) => list.map(_.+:(AssignmentPlanCourseFilter(course)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(AssignmentPlanLabworkFilter(labwork)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: AssignmentPlanProtocol, existingId: Option[UUID]): AssignmentPlanDb = AssignmentPlanDb.from(protocol, existingId)

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

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbidden()
}
