package controllers

import java.util.UUID

import dao._
import database.{CourseDb, CourseTable}
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, God, StudentRole}
import models.{Course, CourseLike, CourseProtocol}
import org.joda.time.DateTime
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.util.{Failure, Try}

object CourseController {
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
  lazy val lecturerAttribute = "lecturer"
  lazy val semesterIndexAttribute = "semesterIndex"
}

@Singleton
final class CourseController @Inject()(cc: ControllerComponents, val abstractDao: CourseDao, val authorityDao: AuthorityDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[CourseProtocol, CourseTable, CourseDb, CourseLike](cc) {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[CourseLike] = CourseLike.writes

  override protected implicit val reads: Reads[CourseProtocol] = CourseProtocol.reads

  override def create(secureContext: SecureContext = contextFrom(Create)) = secureContext asyncAction { implicit request =>
    parsed(
      None,
      course => abstractDao.zip(
        abstractDao.createQuery(course),
        authorityDao.createAssociatedAuthorities(course)
      ).map(_._1)
    ).created
  }

  override def update(id: String, secureContext: SecureContext = contextFrom(Update)) = secureContext asyncAction { implicit request =>
    val uuid = UUID.fromString(id)

    parsed(
      Some(uuid),
      newCourse => {
        for {
          maybeCourse <- abstractDao.getSingle(uuid, atomic = false) if maybeCourse.isDefined
          oldCourse = maybeCourse.map(_.asInstanceOf[Course]).map(toCourseDb).get
          _ <- abstractDao.transactions(abstractDao.updateQuery(newCourse), authorityDao.updateAssociatedAuthorities(oldCourse, newCourse))
        } yield newCourse
      }
    ).jsonResult
  }

  override protected def toDbModel(protocol: CourseProtocol, existingId: Option[UUID]): CourseDb = {
    import utils.date.DateTimeOps.DateTimeConverter
    CourseDb(protocol.label, protocol.description, protocol.abbreviation, protocol.lecturer, protocol.semesterIndex, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import CourseController._
    import dao.CourseDao._

    (attribute, value) match {
      case (`labelAttribute`, l) => l.makeLabelEqualsFilter
      case (`abbreviationAttribute`, a) => a.makeAbbrevFilter
      case (`semesterIndexAttribute`, s) => s.int map semesterIndexFilter
      case (`lecturerAttribute`, l) => l.makeUserFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(EmployeeRole, StudentRole))
    case GetAll => PartialSecureBlock(List(EmployeeRole))
    case Delete => PartialSecureBlock(List(God))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()

  private def toCourseDb(c: Course) = CourseDb(c.label, c.description, c.abbreviation, c.lecturer, c.semesterIndex, id = c.id)
}
