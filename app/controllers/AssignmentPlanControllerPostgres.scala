package controllers

import java.util.UUID

import models._
import play.api.libs.json.{Reads, Writes}
import services.{AbstractDao, AssignmentPlanService, RoleServiceLike, SessionHandlingService}
import store.{AssignmentPlanTable, TableFilter}
import utils.LwmMimeType

import scala.util.Try

final class AssignmentPlanControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val assignmentPlanService: AssignmentPlanService)
  extends AbstractCRUDControllerPostgres[PostgresAssignmentPlanProtocol, AssignmentPlanTable, AssignmentPlanDb, AssignmentPlan] {

  override protected implicit val writes: Writes[AssignmentPlan] = AssignmentPlan.writes

  override protected implicit val reads: Reads[PostgresAssignmentPlanProtocol] = PostgresAssignmentPlan.reads

  override protected val abstractDao: AbstractDao[AssignmentPlanTable, AssignmentPlanDb, AssignmentPlan] = assignmentPlanService

  override protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[AssignmentPlanTable]]]): Try[List[TableFilter[AssignmentPlanTable]]] = ???

  override protected def toDbModel(protocol: PostgresAssignmentPlanProtocol, existingId: Option[UUID]): AssignmentPlanDb = AssignmentPlanDb.from(protocol, existingId)

  override protected def toLwmModel(dbModel: AssignmentPlanDb): AssignmentPlan = dbModel.toAssignmentPlan

  override implicit val mimeType = LwmMimeType.assignmentPlanV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, Permissions.assignmentPlan.create)
    case Update => SecureBlock(restrictionId, Permissions.assignmentPlan.update)
    case Delete => SecureBlock(restrictionId, Permissions.assignmentPlan.delete)
    case Get => SecureBlock(restrictionId, Permissions.assignmentPlan.get)
    case GetAll => SecureBlock(restrictionId, Permissions.assignmentPlan.getAll)
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def deleteFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    delete(id, NonSecureBlock)(request)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request)
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }
}
