package controllers

import java.util.UUID

import dao._
import database.{AssignmentEntryDb, AssignmentEntryTable, AssignmentEntryTypeDb}
import javax.inject.{Inject, Singleton}
import models.{AssignmentEntryLike, AssignmentEntryProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.util.{Failure, Try}

object AssignmentPlanController {
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
}

@Singleton
final class AssignmentPlanController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: AssignmentEntryDao,
  val securedAction: SecurityActionChain
) extends AbstractCRUDController[AssignmentEntryProtocol, AssignmentEntryTable, AssignmentEntryDb, AssignmentEntryLike](cc) {

  import models.Role._

  override protected implicit val writes: Writes[AssignmentEntryLike] = AssignmentEntryLike.writes

  override protected implicit val reads: Reads[AssignmentEntryProtocol] = AssignmentEntryProtocol.reads

  override protected def toDbModel(protocol: AssignmentEntryProtocol, existingId: Option[UUID]): AssignmentEntryDb = {
    val id = existingId.getOrElse(UUID.randomUUID)

    AssignmentEntryDb(
      protocol.labwork,
      protocol.index,
      protocol.label,
      protocol.types.map(t => AssignmentEntryTypeDb(id, t.entryType)),
      protocol.duration,
      id = id
    )
  }

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

  def invalidateFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    invalidate(id, NonSecureBlock)(request)
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
