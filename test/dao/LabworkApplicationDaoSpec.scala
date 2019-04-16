package dao

import java.util.UUID

import database._
import models._
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._

class LabworkApplicationDaoSpec extends AbstractExpandableDaoSpec[LabworkApplicationTable, LabworkApplicationDb, LabworkApplicationLike] {

  import AbstractDaoSpec._
  import utils.date.DateTimeOps.SqlTimestampConverter

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.util.Random.{nextInt, shuffle}

  "A LabworkApplicationDaoSpec also" should {

    "successfully return friends of applicant" in {
      val withFriends = for {
        withFriends <- db.run(dao.filterValidOnly(l => l.friends.exists).take(5).result)
        res <- db.run(DBIO.sequence(withFriends.map(f => dao.friendsOf(f.applicant, f.labwork).result)))
      } yield res

      async(withFriends) { nestedFriends =>
        nestedFriends foreach { friends =>
          friends.map(_.toUniqueEntity) should contain theSameElementsAs expandedStudents.filter(u => friends.exists(_.id == u.id)).map(_.toUniqueEntity)
        }
      }

      val withOutFriends = for {
        withFriends <- db.run(dao.filterValidOnly(l => !l.friends.exists).take(5).result)
        res <- db.run(DBIO.sequence(withFriends.map(f => dao.friendsOf(f.applicant, f.labwork).result)))
      } yield res

      async(withOutFriends) { friends =>
        friends foreach (_ shouldBe empty)
      }
    }
  }

  private val normalLabworks = labworks take 5

  private val expandedLabworks = labworks drop 5

  private val expandedStudents = populateStudents(100)

  private def randomAvoiding(element: UUID, source: List[UUID], atLeastOne: Boolean): Set[UUID] = {
    random(source.filterNot(_ == element), atLeastOne)
  }

  private def random(source: List[UUID], atLeastOne: Boolean): Set[UUID] = {
    val offset = if (atLeastOne) 1 else 0
    shuffle(source).take(nextInt(3) + offset).toSet
  }

  override protected val toAdd: List[LabworkApplicationDb] = for {
    labwork <- expandedLabworks.map(_.id)
    student <- expandedStudents.map(_.id)
    friends = randomAvoiding(student, expandedStudents.map(_.id), atLeastOne = false)
  } yield LabworkApplicationDb(labwork, student, friends)

  override protected val numberOfUpdates: Int = (toAdd.size * 0.1).toInt

  override protected val numberOfDeletions: Int = (toAdd.size * 0.3).toInt

  override protected def update(toUpdate: List[LabworkApplicationDb]): List[LabworkApplicationDb] = {
    val students = expandedStudents map (_.id)

    toUpdate map { lapp =>
      val friends = if (lapp.friends.isEmpty)
        randomAvoiding(lapp.applicant, students, atLeastOne = true)
      else
        lapp.friends.flatMap(f => randomAvoiding(f, students.filterNot(_ == lapp.applicant), atLeastOne = true))

      lapp.copy(friends = friends)
    }
  }

  override protected def atom(dbModel: LabworkApplicationDb): LabworkApplicationLike = {
    val labwork = labworks.find(_.id == dbModel.labwork).get
    val course = courses.find(_.id == labwork.course).get
    val courseAtom = CourseAtom(
      course.label,
      course.description,
      course.abbreviation,
      employees.find(_.id == course.lecturer).get.toUniqueEntity,
      course.semesterIndex,
      course.id
    )

    val labworkAtom = LabworkAtom(
      labwork.label,
      labwork.description,
      semesters.find(_.id == labwork.semester).get.toUniqueEntity,
      courseAtom,
      degrees.find(_.id == labwork.degree).get.toUniqueEntity,
      labwork.subscribable,
      labwork.published,
      labwork.id
    )

    LabworkApplicationAtom(
      labworkAtom,
      (students ++ expandedStudents).find(_.id == dbModel.applicant).get.toUniqueEntity,
      expandedStudents.filter(s => dbModel.friends.contains(s.id)).map(_.toUniqueEntity).toSet,
      dbModel.lastModified.dateTime,
      dbModel.id
    )
  }

  override protected val dao: LabworkApplicationDao = app.injector.instanceOf(classOf[LabworkApplicationDao])

  override protected def name: String = "labworkApplication"

  override protected val dbEntity: LabworkApplicationDb = LabworkApplicationDb(normalLabworks.head.id, students.head.id, Set.empty)

  override protected val invalidDuplicateOfDbEntity: LabworkApplicationDb = dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: LabworkApplicationDb = dbEntity

  override protected val validUpdateOnDbEntity: LabworkApplicationDb = dbEntity.copy(friends = students.slice(1, 3).map(_.id).toSet)

  override protected val dbEntities: List[LabworkApplicationDb] = for {
    labwork <- normalLabworks drop 1
    student <- students drop 1
  } yield LabworkApplicationDb(labwork.id, student.id, Set.empty)

  override protected val lwmAtom: LabworkApplicationLike = atom(dbEntity)

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students ++ expandedStudents),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected def expanderSpecs(dbModel: LabworkApplicationDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = {
    dao.lappFriendQuery.filter(_.labworkApplication === dbModel.id).result.map { friends =>
      friends.map(_.friend) should contain theSameElementsAs (if (isDefined) dbModel.friends else Nil)
    }
  }
}