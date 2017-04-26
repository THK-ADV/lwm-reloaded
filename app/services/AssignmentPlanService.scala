package services

import java.util.UUID

import models.{AssignmentPlan, AssignmentPlanDb}
import slick.lifted.{Rep, TableQuery}
import store.{AssignmentPlanTable, PostgresDatabase, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future
import models.LwmDateTime.DateTimeConverter
import org.joda.time.DateTime

case class AssignmentPlanLabworkFilter(value: String) extends TableFilter[AssignmentPlanTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

trait AssignmentPlanService extends AbstractDao[AssignmentPlanTable, AssignmentPlanDb, AssignmentPlan] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[AssignmentPlanTable]

  override protected def toAtomic(query: Query[AssignmentPlanTable, AssignmentPlanDb, Seq]): Future[Seq[AssignmentPlan]] = ???

  override protected def toUniqueEntity(query: Query[AssignmentPlanTable, AssignmentPlanDb, Seq]): Future[Seq[AssignmentPlan]] = {
    db.run(query.result.map(_.map(_.toAssignmentPlan)))
  }

  override protected def setInvalidated(entity: AssignmentPlanDb): AssignmentPlanDb = {
    val now = DateTime.now.timestamp

    entity.copy(entity.labwork, entity.attendance, entity.mandatory, entity.entries, now, Some(now))
  }

  override protected def existsQuery(entity: AssignmentPlanDb): Query[AssignmentPlanTable, AssignmentPlanDb, Seq] = {
    filterBy(List(AssignmentPlanLabworkFilter(entity.labwork.toString)))
  }

  override protected def shouldUpdate(existing: AssignmentPlanDb, toUpdate: AssignmentPlanDb): Boolean = {
    (existing.attendance != toUpdate.attendance ||
    existing.mandatory != toUpdate.mandatory ||
    existing.entries != toUpdate.entries) &&
    existing.labwork == toUpdate.labwork
  }

  override protected def databaseExpander: Option[DatabaseExpander[AssignmentPlanDb]] = Some(new DatabaseExpander[AssignmentPlanDb] {

    override def expandCreationOf(entities: Seq[AssignmentPlanDb]) = ???

    override def expandDeleteOf(entity: AssignmentPlanDb) = ???

    override def expandUpdateOf(entity: AssignmentPlanDb) = ???
  })
}

object AssignmentPlanService extends AssignmentPlanService with PostgresDatabase
