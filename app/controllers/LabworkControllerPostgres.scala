package controllers

import java.util.UUID

import dao._
import models.Role._
import models._
import play.api.libs.json.{JsValue, Reads, Writes}
import play.api.mvc.{Action, AnyContent}
import services._
import store.{LabworkTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object LabworkControllerPostgres {
  lazy val labelAttribute = "label"
  lazy val degreeAttribute = "degree"
  lazy val semesterAttribute = "semester"
  lazy val courseAttribute = "course"

  lazy val subscribableAttribute = "subscribable"
  lazy val publishedAttribute = "published"
}

final class LabworkControllerPostgres(val sessionService: SessionHandlingService, val authorityDao: AuthorityDao, val abstractDao: LabworkDao) extends
  AbstractCRUDControllerPostgres[PostgresLabworkProtocol, LabworkTable, LabworkDb, Labwork] {

  import controllers.LabworkControllerPostgres._

  override implicit val mimeType: LwmMimeType = LwmMimeType.labworkV1Json

  override protected implicit val writes: Writes[Labwork] = Labwork.writes

  override protected implicit val reads: Reads[PostgresLabworkProtocol] = PostgresLabworkProtocol.reads

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(Student))
    case GetAll => PartialSecureBlock(List(Student))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case Delete => PartialSecureBlock(List(God))
  }

  def allWithDegree(degree: String): Action[AnyContent] = contextFrom(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.append(degreeAttribute -> Seq(degree)))
  }

  def createFrom(course: String): Action[JsValue] = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String): Action[JsValue] = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def allFrom(course: String): Action[AnyContent] = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String): Action[AnyContent] = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  def deleteFrom(course: String, id: String): Action[AnyContent] = restrictedContext(course)(Delete) asyncAction { request =>
    delete(id, NonSecureBlock)(request)
  }

  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[LabworkTable]]]): Try[List[TableFilter[LabworkTable]]] = {
    import controllers.LabworkControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(LabworkLabelFilter(label)))
      case (list, (`degreeAttribute`, degree)) => list.map(_.+:(LabworkDegreeFilter(degree)))
      case (list, (`semesterAttribute`, semester)) => list.map(_.+:(LabworkSemesterFilter(semester)))
      case (list, (`courseAttribute`, course)) => list.map(_.+:(LabworkCourseFilter(course)))
      case (list, (`subscribableAttribute`, subscribable)) => list.map(_.+:(LabworkSubscribableFilter(subscribable)))
      case (list, (`publishedAttribute`, published)) => list.map(_.+:(LabworkPublishedFilter(published)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresLabworkProtocol, existingId: Option[UUID]): LabworkDb = LabworkDb.from(protocol, existingId)
}