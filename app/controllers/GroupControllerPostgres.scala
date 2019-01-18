package controllers

import java.util.UUID

import controllers.helper.GroupingStrategyAttributeFilter
import dao._
import models.Role.{CourseEmployee, CourseManager, God}
import models._
import play.api.libs.json.{Reads, Writes}
import services._
import store.{GroupTable, TableFilter}

import scala.concurrent.Future
import scala.util.{Failure, Try}

//object GroupControllerPostgres {
//  lazy val labworkAttribute = "labwork"
//  lazy val studentAttribute = "student"
//  lazy val labelAttribute = "label"
//}
//
//final class GroupControllerPostgres(val authorityDao: AuthorityDao, val abstractDao: GroupDao, val labworkApplicationService2: LabworkApplicationDao)
//  extends AbstractCRUDControllerPostgres[PostgresGroupProtocol, GroupTable, GroupDb, Group] with GroupingStrategyAttributeFilter {
//
//  import controllers.GroupControllerPostgres._
//
//  import scala.concurrent.ExecutionContext.Implicits.global
//
//  override protected implicit val writes: Writes[Group] = Group.writes
//
//  override protected implicit val reads: Reads[PostgresGroupProtocol] = PostgresGroupProtocol.reads
//
//  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { request =>
//    all(NonSecureBlock)(request.append(labworkAttribute -> Seq(labwork)))
//  }
//
//  def preview(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { request =>
//    (for {
//      applications <- labworkApplicationService2.get(List(LabworkApplicationLabworkFilter(labwork)), atomic = false)
//      apps = applications.map(_.asInstanceOf[PostgresLabworkApplication]).toVector
//      groupingStrategy <- Future.fromTry(strategyOf(request.queryString))
//      groups = GroupService.groupApplicantsBy(groupingStrategy, apps, UUID.fromString(labwork))
//    } yield groups).jsonResult
//  }
//
//  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
//    case Create => SecureBlock(restrictionId, List(CourseManager))
//    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
//    case _ => PartialSecureBlock(List(God))
//  }
//
//  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[GroupTable]]]): Try[List[TableFilter[GroupTable]]] = {
//    (appendTo, (attribute, value)) match {
//      case (list, (`studentAttribute`, student)) => list.map(_.+:(GroupStudentTableFilter(student)))
//      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(GroupLabworkTableFilter(labwork)))
//      case (list, (`labelAttribute`, label)) => list.map(_.+:(GroupLabelTableFilter(label)))
//      case _ => Failure(new Throwable("Unknown attribute"))
//    }
//  }
//
//  override protected def toDbModel(protocol: PostgresGroupProtocol, existingId: Option[UUID]): GroupDb = ???
//
//  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
//    case _ => PartialSecureBlock(List(God))
//  }
//}
