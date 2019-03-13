package dao

import java.util.UUID

import dao.helper.DatabaseExpander
import database._
import javax.inject.Inject
import models._
import slick.jdbc
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

case class AssignmentPlanLabworkFilter(value: String) extends TableFilter[AssignmentPlanTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

case class AssignmentPlanCourseFilter(value: String) extends TableFilter[AssignmentPlanTable] {
  override def predicate = _.memberOfCourse(value)
}

trait AssignmentPlanDao extends AbstractDao[AssignmentPlanTable, AssignmentPlanDb, AssignmentPlanLike] {

  override val tableQuery = TableQuery[AssignmentPlanTable]

  val assignmentEntryQuery: TableQuery[AssignmentEntryTable] = TableQuery[AssignmentEntryTable]
  val assignmentEntryTypeQuery: TableQuery[AssignmentEntryTypeTable] = TableQuery[AssignmentEntryTypeTable]

  override protected def toAtomic(query: Query[AssignmentPlanTable, AssignmentPlanDb, Seq]): Future[Traversable[AssignmentPlanLike]] = collectDependencies(query) {
    case (plan, labwork, entries) => AssignmentPlanAtom(labwork.toUniqueEntity, plan.attendance, plan.mandatory, entries, plan.id)
  }

  private def collectDependencies(query: Query[AssignmentPlanTable, AssignmentPlanDb, Seq])
    (build: (AssignmentPlanDb, LabworkDb, Set[AssignmentEntry]) => AssignmentPlanLike) = {
    def assignmentEntries(entries: Seq[Option[(AssignmentEntryDb, Option[AssignmentEntryTypeDb])]]) = {
      entries.filter(_.isDefined).map(_.get).groupBy(_._1).map {
        case (entry, types) =>
          val entryTypes = types.filter(_._2.isDefined).map { tuple =>
            val db = tuple._2.get
            AssignmentEntryType(db.entryType, db.bool, db.int)
          }

          AssignmentEntry(entry.index, entry.label, entryTypes.toSet, entry.duration)
      }.toSet
    }

    val mandatory = for {
      q <- query
      l <- q.labworkFk
    } yield (q, l)

    val innerJoin = assignmentEntryQuery.joinLeft(assignmentEntryTypeQuery).on(_.id === _.assignmentEntry)
    val outerJoin = mandatory.joinLeft(innerJoin).on(_._1.id === _._1.assignmentPlan)

    val action = outerJoin.result.map(_.groupBy(_._1._1.id).map {
      case (id, dependencies) =>
        val ((assignmentPlan, labwork), _) = dependencies.find(_._1._1.id == id).get
        val entries = assignmentEntries(dependencies.map(_._2))

        build(assignmentPlan, labwork, entries)
    })

    db.run(action)
  }

  override protected def toUniqueEntity(query: Query[AssignmentPlanTable, AssignmentPlanDb, Seq]): Future[Traversable[AssignmentPlanLike]] = collectDependencies(query) {
    case (plan, labwork, entries) => AssignmentPlan(labwork.id, plan.attendance, plan.mandatory, entries, plan.id)
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

    override def expandCreationOf[X <: Effect](entities: AssignmentPlanDb*): jdbc.PostgresProfile.api.DBIOAction[Seq[AssignmentPlanDb], jdbc.PostgresProfile.api.NoStream, Effect.Write with Any] = {
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

      for {
        _ <- assignmentEntryTypeQuery.filter(_.assignmentEntry in entries.map(_.id)).delete
        _ <- entries.delete
      } yield entity
    }

    override def expandUpdateOf(entity: AssignmentPlanDb) = {
      for {
        d <- expandDeleteOf(entity)
        c <- expandCreationOf(d)
      } yield c.head
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

final class AssignmentPlanDaoImpl @Inject()(val db: PostgresProfile.backend.Database, val executionContext: ExecutionContext) extends AssignmentPlanDao