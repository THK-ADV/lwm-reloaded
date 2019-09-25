package controllers

import java.util.UUID

import dao._
import database.{LabworkDb, LabworkTable}
import javax.inject.{Inject, Singleton}
import models.Role._
import models._
import org.joda.time.DateTime
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import security.SecurityActionChain

import scala.util.{Failure, Try}

object LabworkController {
  lazy val labelAttribute = "label"
  lazy val degreeAttribute = "degree"
  lazy val semesterAttribute = "semester"
  lazy val courseAttribute = "course"
  lazy val subscribableAttribute = "subscribable"
  lazy val publishedAttribute = "published"
}

@Singleton
final class LabworkController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: LabworkDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[LabworkProtocol, LabworkTable, LabworkDb, LabworkLike](cc) {

  import controllers.LabworkController._

  override protected implicit val writes: Writes[LabworkLike] = LabworkLike.writes

  override protected implicit val reads: Reads[LabworkProtocol] = LabworkProtocol.reads

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole))
    case GetAll => PartialSecureBlock(List(StudentRole))
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
    all(NonSecureBlock)(request.appending(degreeAttribute -> Seq(degree)))
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def allFrom(course: String): Action[AnyContent] = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String): Action[AnyContent] = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  def invalidateFrom(course: String, id: String): Action[AnyContent] = restrictedContext(course)(Delete) asyncAction { request =>
    invalidate(id, NonSecureBlock)(request)
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import LabworkController._
    import dao.LabworkDao._

    (attribute, value) match {
      case (`labelAttribute`, l) => l.makeLabelEqualsFilter
      case (`degreeAttribute`, d) => d.uuid map degreeFilter
      case (`semesterAttribute`, s) => s.uuid map semesterFilter
      case (`courseAttribute`, c) => c.uuid map courseFilter
      case (`subscribableAttribute`, s) => s.boolean map subscribableFilter
      case (`publishedAttribute`, p) => p.boolean map publishedFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: LabworkProtocol, existingId: Option[UUID]): LabworkDb = {
    import utils.date.DateTimeOps.DateTimeConverter
    LabworkDb(protocol.label, protocol.description, protocol.semester, protocol.course, protocol.degree, protocol.subscribable, protocol.published, DateTime.now.timestamp, None, existingId getOrElse UUID.randomUUID)
  }
}