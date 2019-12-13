package dao

import java.util.UUID

import database._
import models.assignment.{AssignmentEntryAtom, AssignmentEntryLike}
import play.api.inject.guice.GuiceableModule
import slick.jdbc.PostgresProfile.api._

import scala.util.Random.nextInt

final class AssignmentEntryDaoSpec extends AbstractExpandableDaoSpec[AssignmentEntryTable, AssignmentEntryDb, AssignmentEntryLike] {

  import AbstractDaoSpec._
  import AssignmentTypeDb._

  import scala.concurrent.ExecutionContext.Implicits.global

  def assignmentEntries(labwork: LabworkDb, number: Int): Set[AssignmentEntryDb] =
    (0 until number).map { i =>
      val id = UUID.randomUUID
      val ts = (1 until nextInt(3)).map(i => AssignmentTypeDb(id, i.toString, UUID.randomUUID)).toSet
      AssignmentEntryDb(labwork.id, i, i.toString, ts, i, id = id)
    }.toSet

  override protected def name: String = "assignmentEntry"

  override protected val dbEntity: AssignmentEntryDb =
    AssignmentEntryDb(labworks.head.id, 0, "label", Set.empty, 1)

  override protected val invalidDuplicateOfDbEntity: AssignmentEntryDb =
    dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: AssignmentEntryDb =
    dbEntity.copy(index = 1)

  override protected val validUpdateOnDbEntity: AssignmentEntryDb =
    dbEntity.copy(label = "new label", types = Set(AssignmentTypeDb(dbEntity.id, "foo", UUID.randomUUID)))

  override protected val dbEntities: List[AssignmentEntryDb] = labworks.slice(1, 6).tail.zipWithIndex map {
    case (labwork, i) => AssignmentEntryDb(labwork.id, i, i.toString, Set.empty, i)
  }

  override protected val lwmAtom: AssignmentEntryLike = atom(dbEntity)

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )

  override protected val toAdd: List[AssignmentEntryDb] = labworks.drop(6).zip(List(5, 8, 3, 9, 4)).flatMap {
    case (labwork, number) => assignmentEntries(labwork, number)
  }

  override protected val numberOfUpdates: Int = 1

  override protected val numberOfDeletions: Int = 1

  override protected def update(toUpdate: List[AssignmentEntryDb]): List[AssignmentEntryDb] = {
    toUpdate.map { chosen =>
      chosen.copy(types = chosen.types.drop(1) ++ Set(
        AssignmentTypeDb(chosen.id, "type 1", UUID.randomUUID),
        AssignmentTypeDb(chosen.id, "type 2", UUID.randomUUID)
      ))
    }
  }

  override protected def atom(dbModel: AssignmentEntryDb): AssignmentEntryLike =
    AssignmentEntryAtom(
      labworks.find(_.id == dbModel.labwork).get.toUniqueEntity,
      dbModel.index,
      dbModel.label,
      dbModel.types,
      dbModel.duration,
      dbModel.id
    )

  override protected def expanderSpecs(dbModel: AssignmentEntryDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = {
    dao.assignmentEntryTypeQuery.filter(_.assignmentEntry === dbModel.id).result.map { types =>
      types should contain theSameElementsAs (if (isDefined) dbModel.types else Nil)
    }
  }

  override protected val dao: AssignmentEntryDao = app.injector.instanceOf(classOf[AssignmentEntryDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
