package controllers

import java.util.UUID

import models.Permissions.{course, prime}
import models.{Course, CourseDb, PostgresCourse, PostgresCourseProtocol}
import play.api.libs.json.{Reads, Writes}
import services._
import store.{CourseTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object CourseControllerPostgres {
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
  lazy val lecturerAttribute = "lecturer"
  lazy val semesterIndexAttribute = "semesterIndex"
}

final class CourseControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val courseService: CourseService)
  extends AbstractCRUDControllerPostgres[PostgresCourseProtocol, CourseTable, CourseDb, Course] {

  override protected implicit val writes: Writes[Course] = Course.writes

  override protected implicit val reads: Reads[PostgresCourseProtocol] = PostgresCourse.reads

  override protected val abstractDao: AbstractDao[CourseTable, CourseDb, Course] = courseService

  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[CourseTable]]]): Try[List[TableFilter[CourseTable]]] = {
    import controllers.CourseControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(CourseLabelFilter(label)))
      case (list, (`abbreviationAttribute`, abbreviation)) => list.map(_.+:(CourseAbbreviationFilter(abbreviation)))
      case (list, (`semesterIndexAttribute`, semesterIndex)) => list.map(_.+:(CourseSemesterIndexFilter(semesterIndex)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresCourseProtocol, existingId: Option[UUID]): CourseDb = CourseDb.from(protocol, existingId)

  override implicit val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(course.get)
    case GetAll => PartialSecureBlock(course.getAll)
    case _ => PartialSecureBlock(prime)
  }
}
