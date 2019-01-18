package controllers

import java.util.UUID

import dao._
import models._
import play.api.libs.json.{Reads, Writes}
import store.{AssignmentPlanTable, TableFilter}

import scala.util.{Failure, Try}

//object AssignmentPlanControllerPostgres {
//  lazy val labworkAttribute = "labwork"
//  lazy val courseAttribute = "course"
//}
//
//final class AssignmentPlanControllerPostgres(val authorityDao: AuthorityDao, val abstractDao: AssignmentPlanDao) extends AbstractCRUDControllerPostgres[PostgresAssignmentPlanProtocol, AssignmentPlanTable, AssignmentPlanDb, AssignmentPlan] {
//
//  override protected implicit val writes: Writes[AssignmentPlan] = AssignmentPlan.writes
//
//  override protected implicit val reads: Reads[PostgresAssignmentPlanProtocol] = PostgresAssignmentPlanProtocol.reads
//
//  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[AssignmentPlanTable]]]): Try[List[TableFilter[AssignmentPlanTable]]] = {
//    import controllers.AssignmentPlanControllerPostgres._
//
//    (appendTo, (attribute, value)) match {
//      case (list, (`courseAttribute`, course)) => list.map(_.+:(AssignmentPlanCourseFilter(course)))
//      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(AssignmentPlanLabworkFilter(labwork)))
//      case _ => Failure(new Throwable("Unknown attribute"))
//    }
//  }
//
//  override protected def toDbModel(protocol: PostgresAssignmentPlanProtocol, existingId: Option[UUID]): AssignmentPlanDb = AssignmentPlanDb.from(protocol, existingId)
//
//  import models.Role._
//
//  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
//    case Create => SecureBlock(restrictionId, List(CourseManager))
//    case Update => SecureBlock(restrictionId, List(CourseManager))
//    case Delete => SecureBlock(restrictionId, List(CourseManager))
//    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
//    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
//  }
//
//  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
//    create(NonSecureBlock)(request)
//  }
//
//  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
//    update(id, NonSecureBlock)(request)
//  }
//
//  def deleteFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
//    delete(id, NonSecureBlock)(request)
//  }
//
//  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
//    import controllers.AssignmentPlanControllerPostgres.courseAttribute
//
//    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course)))
//  }
//
//  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
//    get(id, NonSecureBlock)(request)
//  }
//}
