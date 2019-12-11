package controllers.assignment

import java.util.UUID

import controllers.AbstractCRUDController
import dao._
import database.{AssignmentEntryDb, AssignmentEntryTable}
import javax.inject.{Inject, Singleton}
import models.{AssignmentEntryLike, AssignmentEntryProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain
import service.AssignmentEntryService

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

object AssignmentEntryController {
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
}

@Singleton
final class AssignmentEntryController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val service: AssignmentEntryService,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[AssignmentEntryProtocol, AssignmentEntryTable, AssignmentEntryDb, AssignmentEntryLike](cc) {

  import models.Role._

  override protected val abstractDao: AbstractDao[AssignmentEntryTable, AssignmentEntryDb, AssignmentEntryLike] = service.dao

  override protected implicit val writes: Writes[AssignmentEntryLike] = AssignmentEntryLike.writes

  override protected implicit val reads: Reads[AssignmentEntryProtocol] = AssignmentEntryProtocol.reads

  override protected def toDbModel(protocol: AssignmentEntryProtocol, existingId: Option[UUID]): AssignmentEntryDb = ???

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
  }

  def takeover(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    val result = for {
      json <- Future.fromTry(unwrap(request))
      srcId = uuid(json.\("srcLabwork"))
      destId = uuid(json.\("destLabwork"))
      created <- service.takeover(srcId, destId)
      atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic
      entries <- if (atomic)
        service.getManyAtomic(created.map(_.id).toList)
      else
        Future.successful(created)
    } yield entries

    result.created
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    val result = for {
      protocol <- Future.fromTry(parseJson(request))
      created <- service.create(protocol)
      atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic
      lwmModel <- if (atomic)
        service.getAtomic(created.id)
      else
        Future.successful(Some(created))
      if lwmModel.isDefined
    } yield lwmModel.get

    result.created
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    val result = for {
      uuid <- id.uuidF
      protocol <- Future.fromTry(parseJson(request))
      updated <- service.update(uuid, protocol)
      atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic
      lwmModel <- if (atomic)
        service.getAtomic(updated.id)
      else
        Future.successful(Some(updated))
      if lwmModel.isDefined
    } yield lwmModel.get

    result.jsonResult
  }

  def invalidateFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { _ =>
    val result = for {
      uuid <- id.uuidF
      invalidated <- service.invalidate(uuid)
    } yield invalidated

    result.jsonResult
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    import AssignmentEntryController.courseAttribute
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import AssignmentEntryController._

    (attribute, value) match {
      case (`courseAttribute`, course) => course.makeCourseFilter
      case (`labworkAttribute`, labwork) => labwork.makeLabworkFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }
}
