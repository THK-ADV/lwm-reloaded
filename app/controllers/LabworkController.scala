package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role._
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import store.{LabworkDb, LabworkTable, TableFilter}
import utils.SecuredAction

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
final class LabworkController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: LabworkDao, val securedAction: SecuredAction)
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
    all(NonSecureBlock)(request.append(degreeAttribute -> Seq(degree)))
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
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
    import controllers.LabworkController._

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

  override protected def toDbModel(protocol: LabworkProtocol, existingId: Option[UUID]): LabworkDb = LabworkDb.from(protocol, existingId)
}