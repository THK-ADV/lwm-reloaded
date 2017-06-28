package controllers

import java.util.UUID

import models.Permissions.timetable
import models.{PostgresTimetable, PostgresTimetableProtocol, Timetable, TimetableDb}
import play.api.libs.json.{Reads, Writes}
import services._
import store.{TableFilter, TimetableTable}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object TimetableControllerPostgres {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
}

final class TimetableControllerPostgres(val roleService: RoleServiceLike, val sessionService: SessionHandlingService, val abstractDao: TimetableService2)
  extends AbstractCRUDControllerPostgres[PostgresTimetableProtocol, TimetableTable, TimetableDb, Timetable] {

  override protected implicit val writes: Writes[Timetable] = Timetable.writes

  override protected implicit val reads: Reads[PostgresTimetableProtocol] = PostgresTimetable.reads

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    update(id, NonSecureBlock)(request)
  }

  override implicit val mimeType = LwmMimeType.timetableV1Json

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, timetable.create)
    case Get => SecureBlock(restrictionId, timetable.get)
    case GetAll => SecureBlock(restrictionId, timetable.getAll)
    case Update => SecureBlock(restrictionId, timetable.update)
    case Delete => SecureBlock(restrictionId, timetable.delete)
  }

  def deleteFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    delete(id, NonSecureBlock)(request)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    import controllers.TimetableControllerPostgres.courseAttribute

    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[TimetableTable]]]): Try[List[TableFilter[TimetableTable]]] = {
    import controllers.TimetableControllerPostgres._

    (appendTo, (attribute, value)) match {
      case (list, (`courseAttribute`, course)) => list.map(_.+:(TimetableCourseFilter(course)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(TimetableLabworkFilter(labwork)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresTimetableProtocol, existingId: Option[UUID]): TimetableDb = TimetableDb.from(protocol, existingId)
}
