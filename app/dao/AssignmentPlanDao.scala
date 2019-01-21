package dao

import java.util.UUID

import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery
import store._

import scala.concurrent.Future

case class AssignmentPlanLabworkFilter(value: String) extends TableFilter[AssignmentPlanTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

case class AssignmentPlanCourseFilter(value: String) extends TableFilter[AssignmentPlanTable] {
  override def predicate = _.labworkFk.map(_.course).filter(_ === UUID.fromString(value)).exists
}

trait AssignmentPlanDao extends AbstractDao[AssignmentPlanTable, AssignmentPlanDb, AssignmentPlan] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[AssignmentPlanTable]

  protected val assignmentEntryQuery: TableQuery[AssignmentEntryTable] = TableQuery[AssignmentEntryTable]
  protected val assignmentEntryTypeQuery: TableQuery[AssignmentEntryTypeTable] = TableQuery[AssignmentEntryTypeTable]

  override protected def toAtomic(query: Query[AssignmentPlanTable, AssignmentPlanDb, Seq]): Future[Seq[AssignmentPlan]] = collectDependencies(query) {
    case (plan, labwork, entries) => PostgresAssignmentPlanAtom(labwork.toLwmModel, plan.attendance, plan.mandatory, entries, plan.id)
  }

  // TODO this is a better implementation than LabworkApplicationService.joinDependencies. test first and adjust when they succeed
  private def collectDependencies(query: Query[AssignmentPlanTable, AssignmentPlanDb, Seq])
    (build: (AssignmentPlanDb, LabworkDb, Set[PostgresAssignmentEntry]) => AssignmentPlan) = {
    def assignmentEntries(entries: Seq[Option[(AssignmentEntryDb, Option[AssignmentEntryTypeDb])]]) = {
      entries.filter(_.isDefined).map(_.get).groupBy(_._1).map {
        case (entry, types) =>
          val entryTypes = types.filter(_._2.isDefined).map { tuple =>
            val db = tuple._2.get
            PostgresAssignmentEntryType(db.entryType, db.bool, db.int)
          }

          PostgresAssignmentEntry(entry.index, entry.label, entryTypes.toSet, entry.duration)
      }.toSet
    }

    val mandatory = for {
      q <- query
      l <- q.joinLabwork
    } yield (q, l)

    val innerJoin = assignmentEntryQuery.joinLeft(assignmentEntryTypeQuery).on(_.id === _.assignmentEntry)
    val outerJoin = mandatory.joinLeft(innerJoin).on(_._1.id === _._1.assignmentPlan)

    val action = outerJoin.result.map(_.groupBy(_._1._1).map {
      case (assignmentPlan, dependencies) =>
        val ((plan, labwork), _) = dependencies.find(_._1._1.id == assignmentPlan.id).get
        val entries = assignmentEntries(dependencies.map(_._2))

        build(plan, labwork, entries)
    }.toSeq)

    db.run(action)
  }

  override protected def toUniqueEntity(query: Query[AssignmentPlanTable, AssignmentPlanDb, Seq]): Future[Seq[AssignmentPlan]] = collectDependencies(query) {
    case (plan, labwork, entries) => PostgresAssignmentPlan(labwork.id, plan.attendance, plan.mandatory, entries, plan.id)
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

    override def expandCreationOf[X <: Effect](entities: Seq[AssignmentPlanDb]) = {
      val assignmentEntries = entities.flatMap(p => p.entries.map { entry =>
        val entryID = UUID.randomUUID
        val types = entry.types.map(t => AssignmentEntryTypeDb(entryID, t.entryType, t.bool, t.int))
        AssignmentEntryDb(p.id, entry.index, entry.label, types, entry.duration, entryID)
      })

      for {
        _ <- assignmentEntryQuery ++= assignmentEntries
        _ <- assignmentEntryTypeQuery ++= assignmentEntries.flatMap(_.types)
      } yield entities
    }

    override def expandDeleteOf(entity: AssignmentPlanDb) = {
      val entries = assignmentEntryQuery.filter(_.assignmentPlan === entity.id)

      val deleted = for {
        d1 <- assignmentEntryTypeQuery.filter(_.assignmentEntry in entries.map(_.id)).delete
        d2 <- entries.delete
      } yield d1 + d2

      deleted.map(_ => Some(entity))
    }

    override def expandUpdateOf(entity: AssignmentPlanDb) = {
      for {
        d <- expandDeleteOf(entity) if d.isDefined
        c <- expandCreationOf(Seq(entity))
      } yield c.headOption
    }
  })

  private lazy val schemas = List(
    tableQuery.schema,
    assignmentEntryQuery.schema,
    assignmentEntryTypeQuery.schema
  )

  override def createSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.map(_.create): _*).transactionally)
  }

  override def dropSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.reverseMap(_.drop): _*).transactionally)
  }
}

final class AssignmentPlanDaoImpl @Inject()(val db: PostgresProfile.backend.Database) extends AssignmentPlanDao