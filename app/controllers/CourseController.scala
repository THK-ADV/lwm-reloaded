package controllers

import java.util.UUID

import dao._
import database.{CourseDb, CourseTable}
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, StudentRole}
import models.{Course, CourseLike, CourseProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import security.SecurityActionChain

import scala.concurrent.Future

object CourseController {
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
//  lazy val lecturerAttribute = "lecturer"
  lazy val semesterIndexAttribute = "semesterIndex"
}

@Singleton
final class CourseController @Inject()(cc: ControllerComponents, val abstractDao: CourseDao, val authorityDao: AuthorityDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[CourseProtocol, CourseTable, CourseDb, CourseLike](cc) {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[CourseLike] = CourseLike.writes

  override protected implicit val reads: Reads[CourseProtocol] = CourseProtocol.reads

  override def create(secureContext: SecureContext = contextFrom(Create)) = secureContext asyncAction { request =>
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      protocol <- Future.fromTry(parseJson(request))
      dbModel = toDbModel(protocol, None)
      _ <- abstractDao.transaction(abstractDao.createQuery(dbModel), authorityDao.createAssociatedAuthorities(dbModel))
      lwmModel <- if (atomic)
        abstractDao.getSingle(dbModel.id, atomic)
      else
        Future.successful(Some(dbModel.toUniqueEntity))
    } yield lwmModel.get).created
  }

  override def update(id: String, secureContext: SecureContext = contextFrom(Update)) = secureContext asyncAction { request =>
    val uuid = UUID.fromString(id)
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      protocol <- Future.fromTry(parseJson(request))
      newCourse = toDbModel(protocol, Some(uuid))
      maybeCourse <- abstractDao.getSingle(uuid, atomic = false) if maybeCourse.isDefined
      oldCourse = maybeCourse.map(_.asInstanceOf[Course]).map(toCourseDb).get
      _ <- abstractDao.transaction(abstractDao.updateQuery(newCourse), authorityDao.updateAssociatedAuthorities(oldCourse, newCourse))
      lwmModel <- if (atomic)
        abstractDao.getSingle(uuid, atomic).map(_.get)
      else
        Future.successful(newCourse.toUniqueEntity)
    } yield lwmModel).updated
  }

  override protected def toDbModel(protocol: CourseProtocol, existingId: Option[UUID]): CourseDb = CourseDb.from(protocol, existingId)

  override def delete(id: String, secureContext: SecureContext = contextFrom(Delete)): Action[AnyContent] = secureContext asyncAction { _ =>
    val uuid = UUID.fromString(id)

    (for {
      course <- abstractDao.getSingle(uuid) if course.isDefined
      courseDb = course.map(_.asInstanceOf[Course]).map(toCourseDb).get
      _ <- abstractDao.transaction(abstractDao.deleteQuery(uuid), authorityDao.deleteAssociatedAuthorities(courseDb))
    } yield course).deleted
  }

//  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[CourseTable]]]): Try[List[TableFilter[CourseTable]]] = {
//    import controllers.CourseController._
//
//    (appendTo, (attribute, values)) match {
//      case (list, (`labelAttribute`, label)) => list.map(_.+:(CourseLabelFilter(label)))
//      case (list, (`abbreviationAttribute`, abbreviation)) => list.map(_.+:(CourseAbbreviationFilter(abbreviation)))
//      case (list, (`semesterIndexAttribute`, semesterIndex)) => list.map(_.+:(CourseSemesterIndexFilter(semesterIndex)))
//      case _ => Failure(new Throwable("Unknown attribute"))
//    }
//  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(EmployeeRole, StudentRole))
    case GetAll => PartialSecureBlock(List(EmployeeRole))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()

  private def toCourseDb(c: Course) = CourseDb(c.label, c.description, c.abbreviation, c.lecturer, c.semesterIndex, id = c.id)
}
