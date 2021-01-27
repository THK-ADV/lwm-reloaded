package controllers

import dao.{AnnotationDao, AuthorityDao}
import database.{AnnotationDb, AnnotationTable}
import models.AnnotationLike.{Annotation, AnnotationAtom}
import models.Role._
import models.{AnnotationLike, AnnotationProtocol, User}
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json.{Json, Reads, Writes, _}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object AnnotationController {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val studentAttribute = "student"
  lazy val systemIdAttribute = "systemId"
  lazy val authorAttribute = "author"
  lazy val reportCardEntryAttribute = "reportCardEntry"

  implicit def annotationWrites: Writes[Annotation] = (
    (JsPath \ "reportCardEntry").write[UUID] and
      (JsPath \ "author").write[UUID] and
      (JsPath \ "message").write[String] and
      (JsPath \ "lastModified").write[DateTime](utils.date.DateTimeJsonFormatter.writeDateTime) and
      (JsPath \ "id").write[UUID]
    ) (unlift(Annotation.unapply))

  implicit def annotationAtomWrites: Writes[AnnotationAtom] = (
    (JsPath \ "reportCardEntry").write[UUID] and
      (JsPath \ "author").write[User](User.writes) and
      (JsPath \ "message").write[String] and
      (JsPath \ "lastModified").write[DateTime](utils.date.DateTimeJsonFormatter.writeDateTime) and
      (JsPath \ "id").write[UUID]
    ) (unlift(AnnotationAtom.unapply))

  implicit def annotationLikeWrites: Writes[AnnotationLike] = {
    case normal: Annotation => Json.toJson(normal)(annotationWrites)
    case atom: AnnotationAtom => Json.toJson(atom)(annotationAtomWrites)
  }

  implicit def protocolReads: Reads[AnnotationProtocol] = Json.reads[AnnotationProtocol]
}

class AnnotationController @Inject()(
  cc: ControllerComponents,
  val abstractDao: AnnotationDao,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[AnnotationProtocol, AnnotationTable, AnnotationDb, AnnotationLike](cc) {

  import AnnotationController._

  override protected implicit val writes: Writes[AnnotationLike] = annotationLikeWrites

  override protected implicit val reads: Reads[AnnotationProtocol] = protocolReads

  override protected def toDbModel(protocol: AnnotationProtocol, existingId: Option[UUID]) = AnnotationDb(
    protocol.reportCardEntry,
    protocol.author,
    protocol.message,
    id = existingId getOrElse UUID.randomUUID
  )

  override protected def restrictedContext(restrictionId: String) = {
    case Create => SecureBlock(restrictionId, List(CourseAssistant, CourseEmployee, CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseAssistant, CourseEmployee, CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseAssistant, CourseEmployee, CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseAssistant, CourseEmployee, CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseAssistant, CourseEmployee, CourseManager))
  }

  override protected def contextFrom = {
    case GetAll => PartialSecureBlock(List(StudentRole))
  }

  def forStudent(labwork: String) = contextFrom(GetAll) asyncAction { request =>
    import utils.Ops.OptionOps

    for {
      systemId <- Future.fromTry(request.systemId.toTry("No User ID found in request"))
      annotations <- all(NonSecureBlock)(request.appending(systemIdAttribute -> Seq(systemId), labworkAttribute -> Seq(labwork)))
    } yield annotations
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def countFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    count(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
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

  override protected def makeTableFilter(attribute: String, value: String) = {
    import dao.helper.TableFilter._

    (attribute, value) match {
      case (`reportCardEntryAttribute`, reportCardEntry) => reportCardEntry.uuid map reportCardEntryFilter
      case (`courseAttribute`, course) => course.uuid map courseByReportCardEntryFilter
      case (`labworkAttribute`, labwork) => labwork.uuid map labworkByReportCardEntryFilter
      case (`studentAttribute`, student) => student.uuid map userByReportCardEntryFilter
      case (`systemIdAttribute`, systemId) => Success(systemIdByReportCardEntryFilter(systemId))
      case (`authorAttribute`, author) => author.uuid map userFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }
}
