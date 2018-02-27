package dao

import java.util.UUID

import models._
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store._

// TODO migrate to abstractExpanderDaoSpec
class LabworkApplicationDaoSpec extends AbstractDaoSpec[LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] with LabworkApplicationDao {

  import dao.AbstractDaoSpec._
  import utils.LwmDateTime.SqlTimestampConverter

  import scala.util.Random.{nextBoolean, nextInt}

  val maxApplicants = 600
  val reservedApplicants = 5
  val maxApplications = 200

  val applicants = (0 until maxApplicants).map(applicant).toList

  val student = populateStudents(1)(degrees).head
  val lapp = labworkApplication(Some(student.id), withFriends = true)

  @scala.annotation.tailrec
  final def randomApplicant(avoiding: Option[UUID] = None): DbUser = {
    val applicant = applicants(nextInt(maxApplicants - reservedApplicants))

    avoiding match {
      case Some(avoid) if applicant.id == avoid => randomApplicant(Some(avoid))
      case _ => applicant
    }
  }

  private def applicant(i: Int): DbUser = DbUser(i.toString, i.toString, i.toString, i.toString, User.StudentType, Some(i.toString), Some(randomDegree.id))

  private def labworkApplication(applicant: Option[UUID] = None, withFriends: Boolean = nextBoolean) = {
    val app = applicant.getOrElse(randomApplicant().id)
    val friends = if (withFriends) (0 until nextInt(2) + 1).map(_ => randomApplicant(Some(app)).id).toSet else Set.empty[UUID]

    LabworkApplicationDb(randomLabwork.id, app, friends)
  }

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ List(student)),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks),
    TableQuery[UserTable].forceInsertAll(applicants)
  )

  override protected def name: String = "labworkApplication"

  override protected val dbEntity: LabworkApplicationDb = labworkApplication(None, withFriends = false)

  override protected val invalidDuplicateOfDbEntity: LabworkApplicationDb = {
    val newFriends = if (dbEntity.friends.isEmpty) Set(randomApplicant(Some(dbEntity.applicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(dbEntity.labwork, dbEntity.applicant, newFriends)
  }

  override protected val invalidUpdateOfDbEntity: LabworkApplicationDb = {
    val newApplicant = randomApplicant(Some(dbEntity.applicant)).id
    val newFriends = if (dbEntity.friends.isEmpty) Set(randomApplicant(Some(newApplicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(dbEntity.labwork, newApplicant, newFriends, dbEntity.lastModified, dbEntity.invalidated, dbEntity.id)
  }

  override protected val validUpdateOnDbEntity: LabworkApplicationDb = {
    val newFriends = if (dbEntity.friends.isEmpty) Set(randomApplicant(Some(dbEntity.applicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(dbEntity.labwork, dbEntity.applicant, newFriends, dbEntity.lastModified, dbEntity.invalidated, dbEntity.id)
  }

  override protected val dbEntities: List[LabworkApplicationDb] = (0 until maxApplications).map(_ => labworkApplication()).toList

  override protected val lwmEntity: LabworkApplication = dbEntity.toLwmModel

  override protected val lwmAtom: LabworkApplication = {
    val labworkAtom = {
      val labwork = labworks.find(_.id == dbEntity.labwork).get
      val semester = semesters.find(_.id == labwork.semester).get
      val course = courses.find(_.id == labwork.course).get
      val lecturer = employees.find(_.id == course.lecturer).get.toLwmModel
      val courseAtom = PostgresCourseAtom(course.label, course.description, course.abbreviation, lecturer, course.semesterIndex, course.id)
      val degree = degrees.find(_.id == labwork.degree).get

      PostgresLabworkAtom(labwork.label, labwork.description, semester.toLwmModel, courseAtom, degree.toLwmModel, labwork.subscribable, labwork.published, labwork.id)
    }

    PostgresLabworkApplicationAtom(
      labworkAtom,
      applicants.find(_.id == dbEntity.applicant).get.toLwmModel,
      Set.empty,
      dbEntity.lastModified.dateTime,
      dbEntity.id
    )
  }

  "A LabworkApplicationService2Spec " should {

    "create a labworkApplication with friends" in {
      val result = await(create(lapp))
      val dbLapp = await(getById(lapp.id.toString, atomic = false))
      val dbFriends = await(db.run(lappFriendQuery.filter(_.labworkApplication === result.id).result))

      result shouldBe lapp
      Some(result.toLwmModel) shouldBe dbLapp
      result.friends shouldBe dbFriends.map(_.friend).toSet
      dbFriends.forall(_.labworkApplication == result.id) shouldBe true
    }

    "update a labworkApplication with friends" in {
      val updated = lapp.copy(lapp.labwork, lapp.applicant, lapp.friends ++ Set(randomApplicant(Some(lapp.applicant)).id))

      val result = await(update(updated)).get
      val dbFriends = await(db.run(lappFriendQuery.filter(_.labworkApplication === result.id).result))

      result shouldBe updated
      result.friends shouldBe dbFriends.map(_.friend).toSet
      dbFriends.forall(_.labworkApplication == result.id) shouldBe true
    }

    "delete a labworkApplication with friends" in {
      // TODO ADJUST
      /*val result = await(delete(lapp)).get
      val dbLapp = await(getById(lapp.id.toString, atomic = false))
      val dbFriends = await(db.run(lappFriendQuery.filter(_.labworkApplication === result.id).result))

      result.id shouldBe lapp.id
      dbLapp shouldBe empty
      dbFriends shouldBe empty*/
    }

    "return a atom of labworkApplication with friends" in {
      def randomLabworkApplicationAtomWith(lapp: LabworkApplicationDb) = {
        val applicant = applicants.find(_.id == lapp.applicant).get
        val friends = applicants.filter(a => lapp.friends.contains(a.id))
        val labworkAtom = {
          val labwork = labworks.find(_.id == lapp.labwork).get
          val semester = semesters.find(_.id == labwork.semester).get
          val degree = degrees.find(_.id == labwork.degree).get
          val course = courses.find(_.id == labwork.course).get
          val lecturer = employees.find(_.id == course.lecturer).get
          val courseAtom = PostgresCourseAtom(course.label, course.description, course.abbreviation, lecturer.toLwmModel, course.semesterIndex, course.id)
          PostgresLabworkAtom(labwork.label, labwork.description, semester.toLwmModel, courseAtom, degree.toLwmModel, labwork.subscribable, labwork.published, labwork.id)
        }

        PostgresLabworkApplicationAtom(labworkAtom, applicant.toLwmModel, friends.map(_.toLwmModel).toSet, lapp.lastModified.dateTime, lapp.id)
      }

      val lapps = applicants.drop(maxApplicants - reservedApplicants).
        map(a => labworkApplication(Some(a.id), withFriends = true))
      val atoms = lapps.map(randomLabworkApplicationAtomWith)

      val createdLapps = await(createMany(lapps))
      val getAtoms = await(getMany(createdLapps.map(_.id).toList))

      createdLapps shouldBe lapps
      (getAtoms ++ atoms).groupBy(_.id).forall {
        case (_, labworkApplications) =>
          val size = labworkApplications.size
          val equals = labworkApplications.head == labworkApplications.last
          size == 2 && equals
      } shouldBe true
    }
  }
}