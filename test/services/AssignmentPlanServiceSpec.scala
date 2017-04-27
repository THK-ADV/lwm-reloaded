package services

import models._
import slick.dbio.Effect
import store._
import slick.driver.PostgresDriver.api._

object AssignmentPlanServiceSpec {
  import scala.util.Random.nextInt
  import services.AbstractDaoSpec.{labworks, maxLabworks}

  def assignmentPlan(labwork: LabworkDb, number: Int) = {
    val types = PostgresAssignmentEntryType.all
    val entries = (0 until number).map { i =>
      PostgresAssignmentEntry(i, i.toString, types.take(nextInt(types.size - 1) + 1), i)
    }.toSet

    AssignmentPlanDb(labwork.id, number, number, entries)
  }

  def atom(plan: AssignmentPlanDb) = PostgresAssignmentPlanAtom(
    labworks.find(_.id == plan.labwork).get.toLabwork,
    plan.attendance,
    plan.mandatory,
    plan.entries,
    plan.id
  )

  val plans = labworks.drop(maxLabworks - 5).zip(List(5, 8, 3, 9, 4)).map {
    case (labwork, number) => assignmentPlan(labwork, number)
  }

  def merge(entries: Seq[AssignmentEntryDb], types: Seq[AssignmentEntryTypeDb]): Set[PostgresAssignmentEntry] = {
    entries.map { e =>
      val entryTypes = types.filter(_.assignmentEntry == e.id).map { t =>
        PostgresAssignmentEntryType(t.entryType, t.bool, t.int)
      }

      PostgresAssignmentEntry(e.index, e.label, entryTypes.toSet, e.duration)
    }.toSet
  }
}

final class AssignmentPlanServiceSpec
  extends AbstractDaoSpec[AssignmentPlanTable, AssignmentPlanDb, AssignmentPlan] with AssignmentPlanService {

  import services.AbstractDaoSpec._
  import services.AssignmentPlanServiceSpec._

  def getEntriesAndTypesFromDb(plan: AssignmentPlanDb): (Seq[AssignmentEntryDb], Seq[AssignmentEntryTypeDb]) = {
    val entryQuery = assignmentEntryQuery.filter(_.assignmentPlan === plan.id)
    val entries = run(entryQuery.result)
    val types = run(assignmentEntryTypeQuery.filter(_.assignmentEntry in entryQuery.map(_.id)).result)

    (entries, types)
  }

  "A AssignmentPlanServiceSpec" should {

    "create assignmentPlans with entries and entry types" in {
      plans.foreach { plan =>
        await(create(plan)) shouldBe plan
        await(getById(plan.id.toString, atomic = false)) shouldBe Some(plan.toAssignmentPlan)
        await(getById(plan.id.toString)) shouldBe Some(atom(plan))

        val (entries, types) = getEntriesAndTypesFromDb(plan)

        plan.entries shouldBe merge(entries, types)
      }
    }

    "update an assignmentPlan with entries and entry types" in {
      val chosen = plans(3)
      val updated = chosen.copy(chosen.labwork, 1, 1, chosen.entries.drop(2) ++ Set(
        PostgresAssignmentEntry(10, 10.toString, PostgresAssignmentEntryType.all.take(1)),
        PostgresAssignmentEntry(11, 11.toString, Set.empty)
      ))

      await(update(updated)) shouldBe Some(updated)
      await(getById(updated.id.toString, atomic = false)) shouldBe Some(updated.toAssignmentPlan)
      await(getById(updated.id.toString)) shouldBe Some(atom(updated))

      val (entries, types) = getEntriesAndTypesFromDb(updated)

      updated.entries shouldBe merge(entries, types)
    }

    "delete an assignmentPlan with entries and entry types" in {
      val deleted = plans(4)

      await(delete(deleted)) shouldBe defined
      await(getById(deleted.id.toString, atomic = false)) shouldBe None
      await(getById(deleted.id.toString)) shouldBe None

      val (entries, types) = getEntriesAndTypesFromDb(deleted)

      entries shouldBe empty
      types shouldBe empty
    }
  }

  override protected def name: String = "assignmentPlan"

  override protected val dbEntity: AssignmentPlanDb = {
    AssignmentPlanDb(labworks.head.id, 5, 5, Set.empty)
  }

  override protected val invalidDuplicateOfDbEntity: AssignmentPlanDb = {
    AssignmentPlanDb(dbEntity.labwork, 10, 10, dbEntity.entries)
  }

  override protected val invalidUpdateOfDbEntity: AssignmentPlanDb = {
    dbEntity.copy(labworks.last.id)
  }

  override protected val validUpdateOnDbEntity: AssignmentPlanDb = {
    val updatedEntries = Set(
      PostgresAssignmentEntry(2, "2", Set(PostgresAssignmentEntryType.Bonus))
    )

    dbEntity.copy(dbEntity.labwork, dbEntity.attendance + 1, dbEntity.mandatory + 1,  updatedEntries)
  }

  override protected val dbEntities: List[AssignmentPlanDb] = assignmentPlans

  override protected val lwmEntity: AssignmentPlan = dbEntity.toAssignmentPlan

  override protected val lwmAtom: AssignmentPlan = PostgresAssignmentPlanAtom(
    labworks.head.toLabwork,
    dbEntity.attendance,
    dbEntity.mandatory,
    dbEntity.entries,
    dbEntity.id
  )

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )
}
