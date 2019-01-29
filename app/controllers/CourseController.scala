package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, StudentRole}
import models.{Course, CourseLike, CourseProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import database.{CourseDb, CourseTable, TableFilter}
import security.SecurityActionChain

import scala.concurrent.Future
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

  override def create(secureContext: SecureContext = contextFrom(Create)) = secureContext asyncAction { request =>
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      protocol <- Future.fromTry(parseJson(request))
      dbModel = toDbModel(protocol, None)
      _ <- abstractDao.transaction(abstractDao.createQuery(dbModel), authorityDao.createByCourseQuery(dbModel))
      lwmModel <- if (atomic)
        abstractDao.getById(dbModel.id.toString, atomic)
      else
        Future.successful(Some(dbModel.toUniqueEntity))
    } yield lwmModel.get).jsonResult
  }

  override def update(id: String, secureContext: SecureContext = contextFrom(Update)) = secureContext asyncAction { request =>
    val uuid = UUID.fromString(id)
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      protocol <- Future.fromTry(parseJson(request))
      dbModel = toDbModel(protocol, Some(uuid))
      oldCourse <- abstractDao.getById(id, atomic = false) if oldCourse.isDefined
      oc = oldCourse.get.asInstanceOf[Course]
      updatedCourse <- abstractDao.update(dbModel)
      _ <- authorityDao.updateByCourse(CourseDb(oc.label, oc.description, oc.abbreviation, oc.lecturer, oc.semesterIndex), dbModel)
      lwmModel <- if (atomic)
        abstractDao.getById(uuid.toString, atomic)
      else
        Future.successful(updatedCourse.map(_.toUniqueEntity))
    } yield lwmModel).jsonResult(uuid)
  }

  override protected def toDbModel(protocol: CourseProtocol, existingId: Option[UUID]): CourseDb = CourseDb.from(protocol, existingId)

  override def delete(id: String, secureContext: SecureContext = contextFrom(Delete)): Action[AnyContent] = secureContext asyncAction { _ =>
    import utils.LwmDateTime._
    val uuid = UUID.fromString(id)

    (for {
      courseBeforeDelete <- abstractDao.getById(id) if courseBeforeDelete.isDefined
      c = courseBeforeDelete.get.asInstanceOf[Course]
      deletedCourse <- abstractDao.delete(uuid) if deletedCourse.isDefined
      _ <- authorityDao.deleteByCourse(CourseDb(c.label, c.description, c.abbreviation, c.lecturer, c.semesterIndex))
    } yield deletedCourse.map(_.dateTime)).jsonResult(uuid)
  }

  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[CourseTable]]]): Try[List[TableFilter[CourseTable]]] = {
    import controllers.CourseController._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(CourseLabelFilter(label)))
      case (list, (`abbreviationAttribute`, abbreviation)) => list.map(_.+:(CourseAbbreviationFilter(abbreviation)))
      case (list, (`semesterIndexAttribute`, semesterIndex)) => list.map(_.+:(CourseSemesterIndexFilter(semesterIndex)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(EmployeeRole, StudentRole))
    case GetAll => PartialSecureBlock(List(EmployeeRole))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
