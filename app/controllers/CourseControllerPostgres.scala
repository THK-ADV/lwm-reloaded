package controllers

import java.util.UUID

import models.Permissions.{course, prime}
import models.{Course, CourseDb, PostgresCourse, PostgresCourseProtocol}
import play.api.libs.json.{JsValue, Reads, Writes}
import play.api.mvc.{Action, AnyContent}
import services._
import store.{CourseTable, TableFilter}
import utils.LwmMimeType
import models.LwmDateTime._

import scala.concurrent.Future
import scala.util.{Failure, Try}

object CourseControllerPostgres {
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
  lazy val lecturerAttribute = "lecturer"
  lazy val semesterIndexAttribute = "semesterIndex"
}

final class CourseControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val abstractDao: CourseService, val authorityService: AuthorityService)
  extends AbstractCRUDControllerPostgres[PostgresCourseProtocol, CourseTable, CourseDb, Course] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[Course] = Course.writes

  override protected implicit val reads: Reads[PostgresCourseProtocol] = PostgresCourse.reads

  override protected def toDbModel(protocol: PostgresCourseProtocol, existingId: Option[UUID]): CourseDb = CourseDb.from(protocol, existingId)

  override implicit val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[CourseTable]]]): Try[List[TableFilter[CourseTable]]] = {
    import controllers.CourseControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(CourseLabelFilter(label)))
      case (list, (`abbreviationAttribute`, abbreviation)) => list.map(_.+:(CourseAbbreviationFilter(abbreviation)))
      case (list, (`semesterIndexAttribute`, semesterIndex)) => list.map(_.+:(CourseSemesterIndexFilter(semesterIndex)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(course.get)
    case GetAll => PartialSecureBlock(course.getAll)
    case _ => PartialSecureBlock(prime)
  }

  override def create(secureContext: SecureContext = contextFrom(Create)): Action[JsValue] = secureContext asyncContentTypedAction { request =>
    (for {
      protocol <- Future.fromTry(parse[PostgresCourseProtocol](request))
      dbModel = toDbModel(protocol, None)
      _ <- abstractDao.transaction(abstractDao.createQuery(dbModel), authorityService.createByCourseQuery(dbModel))
    } yield dbModel.toLwmModel).jsonResult
  }

  override def update(id: String, secureContext: SecureContext = contextFrom(Update)): Action[JsValue] = secureContext asyncContentTypedAction { request =>
    val uuid = UUID.fromString(id)

    (for {
      protocol <- Future.fromTry(parse[PostgresCourseProtocol](request))
      dbModel = toDbModel(protocol, Some(uuid))
      oldCourse <- abstractDao.getById(id, atomic = false) if oldCourse.isDefined
      oc = oldCourse.get.asInstanceOf[PostgresCourse]
      updatedCourse <- abstractDao.update(dbModel)
      _ <- authorityService.updateByCourse(CourseDb(oc.label, oc.description, oc.abbreviation, oc.lecturer, oc.semesterIndex), dbModel)
    } yield updatedCourse.map(_.toLwmModel)).jsonResult(uuid)
  }

  override def delete(id: String, secureContext: SecureContext = contextFrom(Delete)): Action[AnyContent] = secureContext asyncAction   { request =>
    val uuid = UUID.fromString(id)

    (for {
      courseBeforeDelete <- abstractDao.getById(id) if courseBeforeDelete.isDefined
      c = courseBeforeDelete.get.asInstanceOf[PostgresCourse]
      deletedCourse <- abstractDao.delete(uuid) if deletedCourse.isDefined
      _ <- authorityService.deleteByCourse(CourseDb(c.label, c.description, c.abbreviation, c.lecturer, c.semesterIndex))
    } yield deletedCourse.map(_.dateTime)).jsonResult(uuid)
  }
}
