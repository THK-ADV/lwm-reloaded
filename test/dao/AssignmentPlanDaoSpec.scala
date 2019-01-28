package dao

/*
import models._
import slick.jdbc.PostgresProfile.api._
import database._

import scala.util.Random.nextInt

final class AssignmentPlanDaoSpec extends AbstractExpandableDaoSpec[AssignmentPlanTable, AssignmentPlanDb, AssignmentPlanLike] with AssignmentPlanDao {
  import dao.AbstractDaoSpec._

  def assignmentPlan(labwork: LabworkDb, number: Int) = {
    val types = AssignmentEntryType.all
    val entries = (0 until number).map { i =>
      AssignmentEntry(i, i.toString, types.take(nextInt(types.size - 1) + 1), i)
    }.toSet

    database.AssignmentPlanDb(labwork.id, number, number, entries)
  }

  "A AssignmentPlanServiceSpec" should {

    "get assignmentPlans for a given course" in {
      val semester = randomSemester.id
      val degree = randomDegree.id
      val maxLabworksInCourse = 4

      val courses = (0 until 5).map(i =>
        CourseDb(s"label $i", s"desc $i", s"abbrev $i", randomEmployee.id, i)
      )
      val labworks = courses.flatMap(c => (0 until maxLabworksInCourse).map { i =>
        LabworkDb(s"label $i", s"desc $i", semester, c.id, degree)
      })
      val plans = labworks.map(l => assignmentPlan(l, 5))
      val chosenCourse = courses.head

      run(DBIO.seq( // TODO we should setup db testing suites like this
        TableQuery[CourseTable].forceInsertAll(courses),
        TableQuery[LabworkTable].forceInsertAll(labworks)
      ).andThen(
        tableQuery.forceInsertAll(plans)
      ).andFinally(
        filterBy(List(AssignmentPlanCourseFilter(chosenCourse.id.toString))).result.map { dbPlans =>
          dbPlans.size shouldBe maxLabworksInCourse
          dbPlans.map(_.labwork) shouldBe labworks.filter(_.course == chosenCourse.id).map(_.id)
        }
      ))
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
      AssignmentEntry(2, "2", Set(AssignmentEntryType.Bonus))
    )

    dbEntity.copy(dbEntity.labwork, dbEntity.attendance + 1, dbEntity.mandatory + 1,  updatedEntries)
  }

  override protected val dbEntities: List[AssignmentPlanDb] = assignmentPlans

  override protected val lwmEntity: AssignmentPlanLike = dbEntity.toUniqueEntity

  override protected val lwmAtom: AssignmentPlanLike = atom(dbEntity)

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )

  override protected val toAdd: List[AssignmentPlanDb] = labworks.drop(maxLabworks - 5).zip(List(5, 8, 3, 9, 4)).map {
    case (labwork, number) => assignmentPlan(labwork, number)
  }

  override protected val numberOfUpdates: Int = 1

  override protected val numberOfDeletions: Int = 1

  override protected def update(toUpdate: List[AssignmentPlanDb]): List[AssignmentPlanDb] = {
    toUpdate.map { chosen =>
      chosen.copy(chosen.labwork, 1, 1, chosen.entries.drop(2) ++ Set(
        AssignmentEntry(10, 10.toString, AssignmentEntryType.all.take(1)),
        AssignmentEntry(11, 11.toString, Set.empty)
      ))
    }
  }

  override protected def atom(dbModel: AssignmentPlanDb): AssignmentPlanLike = AssignmentPlanAtom(
    labworks.find(_.id == dbModel.labwork).get.toUniqueEntity,
    dbModel.attendance,
    dbModel.mandatory,
    dbModel.entries,
    dbModel.id
  )

  override protected def expanderSpecs(dbModel: AssignmentPlanDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = {
    assignmentEntryQuery.filter(_.assignmentPlan === dbModel.id).joinLeft(assignmentEntryTypeQuery).on(_.id === _.assignmentEntry).result.map(_.groupBy(_._1).map {
      case (entry, values) =>
        val types = values.flatMap(_._2).map(t => AssignmentEntryType(t.entryType,  t.bool, t.int))
        AssignmentEntry(entry.index, entry.label, types.toSet, entry.duration)
    }).map(entries => entries.toSet shouldBe (if (isDefined) dbModel.entries else Set.empty))
  }
}
*/
