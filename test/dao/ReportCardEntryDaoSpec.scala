package dao

import java.util.UUID

import base.{DateGenerator, PostgresDbSpec}
import database._
import play.api.inject.guice.GuiceableModule

class ReportCardEntryDaoSpec extends PostgresDbSpec with DateGenerator {

  import AbstractDaoSpec._
  import profile.api._
  import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter}

  import scala.util.Random.shuffle

  private val dao = app.injector.instanceOf(classOf[ReportCardEntryDao])

  private val labs = populateLabworks(5)(semesters.take(3), courses.take(3), degrees.take(3))

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    runAsync(dao.tableQuery.delete)(_ => ())
  }

  "A ReportCardEntryDaoSpec" should {

    "return email addresses of attendees in given labwork" in {
      val entries = for {
        l <- labs
        s <- shuffle(students) take 20
      } yield ReportCardEntryDb(s.id, l.id, "", randomLocalDate.sqlDate, randomLocalTime.sqlTime, randomLocalTime.sqlTime, randomRoom.id, Set.empty)
      val labwork = labs(2).id

      async(dao.createMany(entries))(_ => Unit)
      async(dao.attendeeEmailAddressesOf(labwork)) { emails =>
        val s = entries.filter(_.labwork == labwork).map(_.student)
        emails should contain theSameElementsAs students.filter(u => s.contains(u.id)).map(_.email)
      }
      async(dao.attendeeEmailAddressesOf(UUID.randomUUID))(_ shouldBe empty)
    }

    "return number of students" in {
      val ls = labs take 2
      val studs = List(students.take(20), students.slice(20, 40))

      def cards(students: List[List[UserDb]]) = {
        def go(labwork: UUID, student: UUID) =
          ReportCardEntryDb(student, labwork, "", randomLocalDate.sqlDate, randomLocalTime.sqlTime, randomLocalTime.sqlTime, randomRoom.id, Set.empty)

        students
          .zip(ls)
          .flatMap {
            case (user, lab) => user.map(u => go(lab.id, u.id))
          }
      }

      val reportCards = cards(studs)

      async(dao.createMany(reportCards))(_ => Unit)
      async(dao.numberOfStudents(ls.head.course, ls.head.id))(_ shouldBe 20)
      async(dao.numberOfStudents(ls(1).course, ls(1).id))(_ shouldBe 20)
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected def dependencies: profile.api.DBIOAction[Unit, profile.api.NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labs)
  )
}
