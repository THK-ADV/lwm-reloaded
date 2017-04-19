package services

import java.util.UUID

import base.PostgresDbSpec
import models._
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store._

final class LabworkApplicationService2Spec extends AbstractDaoSpec[LabworkApplicationTable, LabworkApplicationDb, LabworkApplication, PostgresLabworkApplicationAtom] with LabworkApplicationService2 {
  import services.AbstractDaoSpec._
  import scala.util.Random.{nextInt, nextBoolean}
  import models.LwmDateTime.SqlTimestampConverter

  val maxApplicants = 300
  val reservedApplicants = 5
  val maxApplications = 100

  val applicants = (0 until maxApplicants).map(applicant).toList

  @scala.annotation.tailrec
  def randomApplicant(avoiding: Option[UUID] = None): DbUser = {
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
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[LabworkTable].forceInsertAll(labworks),
    TableQuery[UserTable].forceInsertAll(applicants)
  )

  override protected val labworkApplicationFriendService: LabworkApplicationFriendService = new LabworkApplicationFriendServiceSpec()

  override protected def name: String = "labworkApplication"

  override protected val entity: LabworkApplicationDb = labworkApplication(None, withFriends = false)

  override protected val invalidDuplicateOfEntity: LabworkApplicationDb = {
    val newFriends = if (entity.friends.isEmpty) Set(randomApplicant(Some(entity.applicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(entity.labwork, entity.applicant, newFriends)
  }

  override protected val invalidUpdateOfEntity: LabworkApplicationDb = {
    val newApplicant = randomApplicant(Some(entity.applicant)).id
    val newFriends = if (entity.friends.isEmpty) Set(randomApplicant(Some(newApplicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(entity.labwork, newApplicant, newFriends, entity.timestamp, entity.lastModified, entity.invalidated, entity.id)
  }

  override protected val validUpdateOnEntity: LabworkApplicationDb = {
    val newFriends = if (entity.friends.isEmpty) Set(randomApplicant(Some(entity.applicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(entity.labwork, entity.applicant, newFriends, entity.timestamp, entity.lastModified, entity.invalidated, entity.id)
  }

  override protected val entities: List[LabworkApplicationDb] = (0 until maxApplications).map(_ => labworkApplication()).toList

  override protected val postgresEntity: LabworkApplication = entity.toLabworkApplication

  override protected val postgresAtom: PostgresLabworkApplicationAtom = {
    val labworkAtom = {
      val labwork = labworks.find(_.id == entity.labwork).get
      val semester = semesters.find(_.id == labwork.semester).get
      val course = courses.find(_.id == labwork.course).get
      val lecturer = employees.find(_.id == course.lecturer).get.toUser
      val courseAtom = PostgresCourseAtom(course.label, course.description, course.abbreviation, lecturer, course.semesterIndex, course.id)
      val degree = degrees.find(_.id == labwork.degree).get

      PostgresLabworkAtom(labwork.label, labwork.description, semester.toSemester, courseAtom, degree.toDegree, labwork.subscribable, labwork.published, labwork.id)
    }

    PostgresLabworkApplicationAtom(
      labworkAtom,
      applicants.find(_.id == entity.applicant).get.toUser,
      Set.empty,
      entity.timestamp.dateTime,
      entity.id
    )
  }

  "A LabworkApplicationService2Spec " should {

    val lapp = labworkApplication(Some(entity.applicant), withFriends = true)

    "create a labworkApplication with friends" in {
      val result = await(create(lapp))
      val dbLapp = await(db.run(filterBy(List(LabworkApplicationIdFilter(lapp.id.toString))).result.headOption))
      val dbFriends = await(db.run(labworkApplicationFriendService.tableQuery.filter(_.labworkApplication === result.id).result))

      result shouldBe lapp
      Some(result.copy(result.labwork, result.applicant, Set.empty)) shouldBe dbLapp
      result.friends shouldBe dbFriends.map(_.friend).toSet
      dbFriends.forall(_.labworkApplication == result.id) shouldBe true
    }

    "update a labworkApplication with friends" in {
      val updated = lapp.copy(lapp.labwork, lapp.applicant, lapp.friends ++ Set(randomApplicant(Some(lapp.applicant)).id))

      val result = await(update(updated)).get
      val dbFriends = await(db.run(labworkApplicationFriendService.tableQuery.filter(_.labworkApplication === result.id).result))

      result shouldBe updated
      result.friends shouldBe dbFriends.map(_.friend).toSet
      dbFriends.forall(_.labworkApplication == result.id) shouldBe true
    }

    "delete a labworkApplication with friends" in {
      val result = await(delete(lapp)).get
      val dbLapp = await(get(List(LabworkApplicationIdFilter(lapp.id.toString)), atomic = false))
      val dbFriends = await(db.run(labworkApplicationFriendService.tableQuery.filter(_.labworkApplication === result.id).result))

      result.id shouldBe lapp.id
      dbLapp shouldBe empty
      dbFriends shouldBe empty
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
          val courseAtom = PostgresCourseAtom(course.label, course.description, course.abbreviation, lecturer.toUser, course.semesterIndex, course.id)
          PostgresLabworkAtom(labwork.label, labwork.description, semester.toSemester, courseAtom, degree.toDegree, labwork.subscribable, labwork.published, labwork.id)
        }

        PostgresLabworkApplicationAtom(labworkAtom, applicant.toUser, friends.map(_.toUser).toSet, lapp.timestamp.dateTime, lapp.id)
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

final class LabworkApplicationFriendServiceSpec extends PostgresDbSpec with LabworkApplicationFriendService {
  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = ???
}