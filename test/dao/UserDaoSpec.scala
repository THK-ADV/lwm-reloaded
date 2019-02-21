package dao

import java.util.UUID

import dao.helper.{Created, Updated}
import database._
import database.helper.{EmployeeStatus, LdapUserStatus, LecturerStatus, StudentStatus}
import models._
import models.helper.{Allowed, Almost, Denied, NotExisting}
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

final class UserDaoSpec extends AbstractDaoSpec[UserTable, UserDb, User] {

  import AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._

  import scala.util.Random.nextInt

  val maxUser = 200
  val maxDegrees = 3

  val degrees: List[DegreeDb] = {
    (0 until maxDegrees).map(i => DegreeDb(i.toString, i.toString)).toList
  }

  val dbUser: List[UserDb] = {
    def randomStatus = scala.util.Random.shuffle(List(StudentStatus, EmployeeStatus, LecturerStatus)).head

    def studentAttributes(status: LdapUserStatus, number: Int) = {
      if (status == StudentStatus)
        (Some(number.toString), Some(degrees(nextInt(maxDegrees)).id))
      else
        (None, None)
    }

    (0 until maxUser).map { i =>
      val status = randomStatus
      val (regId, enrollment) = studentAttributes(status, i)

      UserDb(i.toString, i.toString, i.toString, i.toString, status, regId, enrollment)
    }.toList
  }

  val chosenDegree = degrees.last

  val semester = populateSemester(1).head

  val employee = UserDb("", "", "", "", EmployeeStatus, None, None)

  val course = CourseDb("", "", "", employee.id, 1)

  val labwork = LabworkDb("label", "desc", semester.id, course.id, chosenDegree.id)

  val otherLabwork = labwork.copy(id = UUID.randomUUID)

  "A UserServiceSpec " should {

    "return a user by id (either atomic or non atomic)" in {
      val student = dbUser.find(_.status == StudentStatus)
      val studentAtom = student.map { u =>
        val d = u.enrollment.flatMap(id => degrees.find(_.id == id)).get.toUniqueEntity
        StudentAtom(u.systemId, u.lastname, u.firstname, u.email, u.registrationId.get, d, u.id)
      }

      val employee = dbUser.find(_.status == EmployeeStatus)
      val employeeAtom = employee.map(e => Employee(e.systemId, e.lastname, e.firstname, e.email, e.id))

      async(dao.get(List(UserIdFilter(student.get.id.toString)), atomic = false))(_.headOption shouldBe student.map(_.toUniqueEntity))
      async(dao.get(List(UserIdFilter(employee.get.id.toString)), atomic = false))(_.headOption shouldBe employee.map(_.toUniqueEntity))
      async(dao.get(List(UserIdFilter(studentAtom.get.id.toString))))(_.headOption shouldBe studentAtom)
      async(dao.get(List(UserIdFilter(employeeAtom.get.id.toString))))(_.headOption shouldBe employeeAtom)
    }

    "filter users by certain attributes" in {
      val degree = degrees(nextInt(maxDegrees))
      val possibleUsers1 = dbUser.filter(u => u.status == StudentStatus && u.enrollment.get == degree.id).map(_.toUniqueEntity)
      val possibleUsers2 = dbUser.filter(u => u.firstname.contains("5") && u.lastname.contains("5")).map(_.toUniqueEntity)
      val possibleUsers3 = dbUser.filter(_.systemId == "10").map(_.toUniqueEntity)

      async(dao.get(List(
        UserStatusFilter(StudentStatus.label),
        UserDegreeFilter(degree.id.toString)
      ), atomic = false))(_ shouldBe possibleUsers1)

      async(dao.get(List(
        UserFirstnameFilter("5"),
        UserLastnameFilter("5")
      ), atomic = false))(_ shouldBe possibleUsers2)

      async(dao.get(List(
        UserSystemIdFilter("10")
      ), atomic = false))(_ shouldBe possibleUsers3)

      async(dao.get(List(
        UserFirstnameFilter("3"),
        UserLastnameFilter("3"),
        UserSystemIdFilter("4")
      ), atomic = false))(_ shouldBe empty)
    }

    "make a student by given properties" in {
      val degree = degrees(nextInt(maxDegrees))
      val query = dao.makeUser("make student systemId", "make student last", "make student first", "make student email", StudentStatus.label, Some("make student regId"), Some(degree.abbreviation))

      runAsync(query) { user =>
        user.systemId shouldBe "make student systemId"
        user.lastname shouldBe "make student last"
        user.firstname shouldBe "make student first"
        user.email shouldBe "make student email"
        user.status shouldBe StudentStatus
        user.registrationId.value shouldBe "make student regId"
        user.enrollment.value shouldBe degree.id
      }
    }

    "fail to make any user if properties are wrong" in {
      val query1 = dao.makeUser("make student systemId", "make student last", "make student first", "make student email", StudentStatus.label, None, None)
      val query2 = dao.makeUser("make student systemId", "make student last", "make student first", "make student email", StudentStatus.label, Some("x"), None)
      val query3 = dao.makeUser("make student systemId", "make student last", "make student first", "make student email", StudentStatus.label, None, Some("x"))

      List(query1, query2, query3) foreach { action =>
        runAsync(action.failed)(_.getLocalizedMessage.containsSlice("must have a associated registration-id and degree abbreviation") shouldBe true)
      }
    }

    "fail to make a student if status is wrong" in {
      val degree = degrees(nextInt(maxDegrees))
      val query = dao.makeUser("make student systemId", "make student last", "make student first", "make student email", "bad status", Some("make student regId"), Some(degree.abbreviation))

      runAsync(query.failed) { throwable =>
        throwable.getLocalizedMessage.containsSlice("status") shouldBe true
      }
    }

    "fail to make a student if degree abbreviation is not found" in {
      val query = dao.makeUser("make student systemId", "make student last", "make student first", "make student email", StudentStatus.label, Some("make student regId"), Some("invalid abbrev"))

      runAsync(query.failed) { throwable =>
        throwable.getLocalizedMessage.containsSlice("'invalid abbrev' not found") shouldBe true
      }
    }

    "make a employee by given properties" in {
      val query = dao.makeUser("make employee systemId", "make employee last", "make employee first", "make employee email", EmployeeStatus.label, None, None)

      runAsync(query) { user =>
        user.systemId shouldBe "make employee systemId"
        user.lastname shouldBe "make employee last"
        user.firstname shouldBe "make employee first"
        user.email shouldBe "make employee email"
        user.status shouldBe EmployeeStatus
        user.registrationId shouldBe empty
        user.enrollment shouldBe empty
      }
    }

    "make a lecturer by given properties" in {
      val query = dao.makeUser("make lecturer systemId", "make lecturer last", "make lecturer first", "make lecturer email", LecturerStatus.label, None, None)

      runAsync(query) { user =>
        user.systemId shouldBe "make lecturer systemId"
        user.lastname shouldBe "make lecturer last"
        user.firstname shouldBe "make lecturer first"
        user.email shouldBe "make lecturer email"
        user.status shouldBe LecturerStatus
        user.registrationId shouldBe empty
        user.enrollment shouldBe empty
      }
    }

    "create a student with dedicated basic authority" in {
      val degree = degrees(nextInt(maxDegrees))
      val user = UserDb("student systemId", "student last", "student first", "student email", StudentStatus, Some("regId"), Some(degree.id))

      runAsync(dao.createWithBasicAuthorityQuery(user)) {
        case (createdUser, createdAuth) =>
          createdUser shouldBe user
          createdAuth.user shouldBe user.id
          createdAuth.course shouldBe empty
      }
    }

    "create a employee with dedicated basic authority" in {
      val user = UserDb("employee systemId", "employee last", "employee first", "employee email", EmployeeStatus, None, None)

      runAsync(dao.createWithBasicAuthorityQuery(user)) {
        case (createdUser, createdAuth) =>
          createdUser shouldBe user
          createdAuth.user shouldBe user.id
          createdAuth.course shouldBe empty
      }
    }

    "create a lecturer with dedicated basic authority" in {
      val user = UserDb("lecturer systemId", "lecturer last", "lecturer first", "lecturer email", LecturerStatus, None, None)

      runAsync(dao.createWithBasicAuthorityQuery(user)) {
        case (createdUser, createdAuth) =>
          createdUser shouldBe user
          createdAuth.user shouldBe user.id
          createdAuth.course shouldBe empty
      }
    }

    "create and update student with dedicated basic authority" in {
      val degree = degrees(nextInt(maxDegrees))

      val user = UserDb("another student systemId", "another student last", "another student first", "another student email", StudentStatus, Some("another regId"), Some(degree.id))
      async(dao.createOrUpdateWithBasicAuthority(user))(_ shouldBe Created(user))

      val updated = user.copy(lastname = "updated student last", email = "updated student email")
      async(dao.createOrUpdateWithBasicAuthority(updated))(_ shouldBe Updated(updated))
    }

    "create and update employee with dedicated basic authority" in {
      val user = UserDb("another employee systemId", "another employee last", "another employee first", "another employee email", EmployeeStatus, None, None)
      async(dao.createOrUpdateWithBasicAuthority(user))(_ shouldBe Created(user))

      val updated = user.copy(lastname = "updated employee last", email = "updated employee email")
      async(dao.createOrUpdateWithBasicAuthority(updated))(_ shouldBe Updated(updated))
    }

    "create and update lecturer with dedicated basic authority" in {
      val user = UserDb("another lecturer systemId", "another lecturer last", "another lecturer first", "another lecturer email", LecturerStatus, None, None)
      async(dao.createOrUpdateWithBasicAuthority(user))(_ shouldBe Created(user))

      val updated = user.copy(lastname = "updated lecturer last", email = "updated lecturer email")
      async(dao.createOrUpdateWithBasicAuthority(updated))(_ shouldBe Updated(updated))
    }

    "deny buddy requests properly" in {
      val student = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id)).get
      val buddy = dbUser.find(u => u.status == StudentStatus && !u.enrollment.contains(chosenDegree.id)).get
      val invalidBuddy = "not in system"

      async(dao.buddyResult(student.id.toString, invalidBuddy, labwork.id.toString))(_ shouldBe NotExisting(invalidBuddy))
      async(dao.buddyResult(student.id.toString, buddy.systemId, labwork.id.toString))(_ shouldBe Denied(buddy.toUniqueEntity))
    }

    "allow a buddy request" in {
      val student = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id)).get
      val buddy = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id) && u.id != student.id).get
      val someoneElse = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id) && !List(student.id, buddy.id).contains(u.id)).get
      val someoneElse2 = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id) && !List(student.id, buddy.id, someoneElse.id).contains(u.id)).get

      val lapps = List(
        LabworkApplicationDb(labwork.id, someoneElse.id, Set(someoneElse2.id)),
        LabworkApplicationDb(labwork.id, someoneElse2.id, Set(someoneElse.id)),
        LabworkApplicationDb(labwork.id, buddy.id, Set(student.id))
      )

      async(dao.labworkApplicationDao.createMany(lapps))(_ should not be empty)
      async(dao.buddyResult(student.id.toString, buddy.systemId, labwork.id.toString))(_ shouldBe Allowed(buddy.toUniqueEntity))
      async(dao.labworkApplicationDao.deleteMany(lapps.map(_.id)))(_.size shouldBe lapps.size)
    }

    "almost allow a buddy request" in {
      val student = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id)).get
      val buddy = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id) && u.id != student.id).get
      val someoneElse = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id) && !List(student.id, buddy.id).contains(u.id)).get
      val someoneElse2 = dbUser.find(u => u.status == StudentStatus && u.enrollment.contains(chosenDegree.id) && !List(student.id, buddy.id, someoneElse.id).contains(u.id)).get

      val lapps = List(
        LabworkApplicationDb(labwork.id, someoneElse.id, Set(someoneElse2.id)),
        LabworkApplicationDb(labwork.id, someoneElse2.id, Set(someoneElse.id)),
        LabworkApplicationDb(otherLabwork.id, buddy.id, Set(student.id))
      )

      async(dao.labworkApplicationDao.createMany(lapps))(_ should not be empty)
      async(dao.buddyResult(student.id.toString, buddy.systemId, labwork.id.toString))(_ shouldBe Almost(buddy.toUniqueEntity))
    }
  }

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[SemesterTable].forceInsert(semester),
    TableQuery[UserTable].forceInsert(employee),
    TableQuery[CourseTable].forceInsert(course),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[LabworkTable].forceInsertAll(List(labwork, otherLabwork)),
    TableQuery[RoleTable].forceInsertAll(roles)
  )

  override protected def name: String = "user"

  override protected val dbEntity: UserDb = UserDb("delete", "delete", "delete", "delete", StudentStatus, Some("regId"), Some(degrees.head.id))

  override protected val dbEntities: List[UserDb] = dbUser

  override protected val invalidDuplicateOfDbEntity: UserDb = dbEntity.copy(lastname = "other", firstname = "other", email = "other")

  override protected val invalidUpdateOfDbEntity: UserDb = dbEntity.copy(systemId = "new SystemId")

  override protected val validUpdateOnDbEntity: UserDb = dbEntity.copy(lastname = "married", enrollment = Some(degrees.last.id))

  override protected val lwmAtom: User = StudentAtom(
    dbEntity.systemId,
    dbEntity.lastname,
    dbEntity.firstname,
    dbEntity.email,
    dbEntity.registrationId.get,
    degrees.find(_.id == dbEntity.enrollment.get).get.toUniqueEntity,
    dbEntity.id
  )

  override protected val dao: UserDao = app.injector.instanceOf(classOf[UserDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
