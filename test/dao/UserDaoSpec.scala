package dao

/*
import java.util.UUID

import models._
import services._
import services.ldap.LdapUser
import slick.dbio.Effect.Write
import slick.driver
import slick.driver.PostgresDriver
import database._

final class UserDaoSpec extends AbstractDaoSpec[UserTable, UserDb, User] with UserDao {
  import slick.jdbc.PostgresProfile.api._
  import dao.AbstractDaoSpec._
  import scala.util.Random.nextInt

  val maxUser = 200
  val maxDegrees = 3

  val degrees: List[DegreeDb] = {
    (0 until maxDegrees).map(i => DegreeDb(i.toString, i.toString)).toList
  }

  val dbUser: List[UserDb] = {
    def randomStatus = User.types(nextInt(3))
    def studentAttributes(status: String, number: Int) = {
      if (status == User.StudentType)
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
  val employee = UserDb("", "", "", "", User.EmployeeType, None, None)
  val course = CourseDb("", "", "", employee.id, 1)
  val labwork = LabworkDb("label", "desc", semester.id, course.id, chosenDegree.id)
  val otherLabwork = labwork.copy(id = UUID.randomUUID)

  "A UserServiceSpec " should {
    "return a user by id (either atomic or non atomic)" in {
      val student = dbUser.find(_.status == User.StudentType)
      val studentAtom = student.map { u =>
        val d = u.enrollment.flatMap(id => degrees.find(_.id == id)).get.toUniqueEntity
        StudentAtom(u.systemId, u.lastname, u.firstname, u.email, u.registrationId.get, d, u.id)
      }

      val employee = dbUser.find(_.status == User.EmployeeType)
      val employeeAtom = employee.map(e => Employee(e.systemId, e.lastname, e.firstname, e.email, e.id))

      await(get(List(UserIdFilter(student.get.id.toString)), atomic = false)).headOption shouldBe student.map(_.toUniqueEntity)
      await(get(List(UserIdFilter(employee.get.id.toString)), atomic = false)).headOption shouldBe employee.map(_.toUniqueEntity)
      await(get(List(UserIdFilter(studentAtom.get.id.toString)))).headOption shouldBe studentAtom
      await(get(List(UserIdFilter(employeeAtom.get.id.toString)))).headOption shouldBe employeeAtom
    }

    "filter users by certain attributes" in {
      val degree = degrees(nextInt(maxDegrees))
      val possibleUsers1 = dbUser.filter(u => u.status == User.StudentType && u.enrollment.get == degree.id).map(_.toUniqueEntity)
      val possibleUsers2 = dbUser.filter(u => u.firstname.contains("5") && u.lastname.contains("5")).map(_.toUniqueEntity)
      val possibleUsers3 = dbUser.filter(_.systemId == "10").map(_.toUniqueEntity)

      await(get(List(
        UserStatusFilter(User.StudentType),
        UserDegreeFilter(degree.id.toString)
      ), atomic = false)) shouldBe possibleUsers1

      await(get(List(
        UserFirstnameFilter("5"),
        UserLastnameFilter("5")
      ), atomic = false)) shouldBe possibleUsers2

      await(get(List(
        UserSystemIdFilter("10")
      ), atomic = false)) shouldBe possibleUsers3

      await(get(List(
        UserFirstnameFilter("3"),
        UserLastnameFilter("3"),
        UserSystemIdFilter("4")
      ), atomic = false)) shouldBe empty
    }

    "create a user from ldap when not existing" in {
      val degree = degrees.head
      val ldapUser = LdapUser("systemId", "firstname", "lastname", "email", User.EmployeeType, None, None)
      val ldapUser2 = LdapUser("systemId2", "firstname2", "lastname2", "email2", User.StudentType, Some("regId"), Some(degree.label))
      val dbVariation = UserDb(ldapUser.systemId, ldapUser.lastname, ldapUser.firstname, ldapUser.email, ldapUser.status, None, None, lastModified, None)
      val dbVariation2 = UserDb(ldapUser2.systemId, ldapUser2.lastname, ldapUser2.firstname, ldapUser2.email, ldapUser2.status, ldapUser2.registrationId, Some(degree.id), lastModified, None)

      val (user, maybeAuth) = await(createOrUpdate(ldapUser))
      val (user2, maybeAuth2) = await(createOrUpdate(ldapUser2))
      val allUser = await(get())
      val allAuths = await(authorityService.get())
      val maybeAuths = List(maybeAuth, maybeAuth2)

      toDbUser(user) shouldBe dbVariation.copy(id = user.id)
      toDbUser(user2) shouldBe dbVariation2.copy(id = user2.id)
      allUser.size shouldBe dbUser.size + 2 + 1
      maybeAuths.forall(_.isDefined) shouldBe true
      maybeAuths.map(_.get).forall(a => allAuths.contains(a)) shouldBe true
    }

    "update a user from ldap when already exists" in {
      val chosenEmployee = dbUser.find(_.status == User.EmployeeType).get
      val chosenStudent = dbUser.find(_.status == User.StudentType).get
      val degree = chosenStudent.enrollment.flatMap(current => degrees.find(_.id == current))

      val ldapEmployee = LdapUser(chosenEmployee.systemId, "updateFirst", "updateLast", chosenEmployee.email, chosenEmployee.status, None, None)
      val dbEmployee = UserDb(ldapEmployee.systemId, ldapEmployee.lastname, ldapEmployee.firstname, ldapEmployee.email, ldapEmployee.status, None, None, lastModified, None, chosenEmployee.id)
      val ldapStudent = LdapUser(chosenStudent.systemId, "updateFirst2", chosenStudent.lastname, "updateEmail", chosenStudent.status, chosenStudent.registrationId, degree.map(_.abbreviation))
      val dbStudent = UserDb(ldapStudent.systemId, ldapStudent.lastname, ldapStudent.firstname, ldapStudent.email, ldapStudent.status, ldapStudent.registrationId, degree.map(_.id), lastModified, None, chosenStudent.id)

      val (employee, maybeAuth) = await(createOrUpdate(ldapEmployee))
      val (student, maybeAuth2) = await(createOrUpdate(ldapStudent))

      toDbUser(employee) shouldBe dbEmployee
      toDbUser(student) shouldBe dbStudent
      List(maybeAuth, maybeAuth2).forall(_.isEmpty) shouldBe true
    }

    "deny buddy requests properly" in {
      val student = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id)).get
      val buddy = dbUser.find(u => u.status == User.StudentType && !u.enrollment.contains(chosenDegree.id)).get
      val invalidBuddy = "not in system"

      await(buddyResult(student.id.toString, invalidBuddy, labwork.id.toString)) shouldBe NotExisting(invalidBuddy)
      await(buddyResult(student.id.toString, buddy.systemId, labwork.id.toString)).isInstanceOf[Denied] shouldBe true
    }

    "allow a buddy request" in {
      val student = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id)).get
      val buddy = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id) && u.id != student.id).get
      val someoneElse = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id) && !List(student.id, buddy.id).contains(u.id)).get
      val someoneElse2 = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id) && !List(student.id, buddy.id, someoneElse.id).contains(u.id)).get

      val lapps = List(
        LabworkApplicationDb(labwork.id, someoneElse.id, Set(someoneElse2.id)),
        LabworkApplicationDb(labwork.id, someoneElse2.id, Set(someoneElse.id)),
        LabworkApplicationDb(labwork.id, buddy.id, Set(student.id))
      )

      await(labworkApplicationService.createMany(lapps)) should not be empty
      await(buddyResult(student.id.toString, buddy.systemId, labwork.id.toString)).isInstanceOf[Allowed] shouldBe true
      await(labworkApplicationService.deleteMany(lapps.map(_.id))).count(_.isDefined) shouldBe lapps.size
    }

    "almost allow a buddy request" in {
      val student = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id)).get
      val buddy = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id) && u.id != student.id).get
      val someoneElse = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id) && !List(student.id, buddy.id).contains(u.id)).get
      val someoneElse2 = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id) && !List(student.id, buddy.id, someoneElse.id).contains(u.id)).get

      val lapps = List(
        LabworkApplicationDb(labwork.id, someoneElse.id, Set(someoneElse2.id)),
        LabworkApplicationDb(labwork.id, someoneElse2.id, Set(someoneElse.id)),
        LabworkApplicationDb(otherLabwork.id, buddy.id, Set(student.id))
      )

      await(labworkApplicationService.createMany(lapps)) should not be empty
      await(buddyResult(student.id.toString, buddy.systemId, labwork.id.toString)).isInstanceOf[Almost] shouldBe true
    }
  }

  private def toDbUser(user: User) = user match {
    case s: Student =>
      UserDb(s.systemId, s.lastname, s.firstname, s.email, User.StudentType, Some(s.registrationId), Some(s.enrollment), lastModified, None, s.id)
    case e: Employee =>
      UserDb(e.systemId, e.lastname, e.firstname, e.email, User.EmployeeType, None, None, lastModified, None, e.id)
    case l: Lecturer =>
      UserDb(l.systemId, l.lastname, l.firstname, l.email, User.LecturerType, None, None, lastModified, None, l.id)
  }

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[SemesterTable].forceInsert(semester),
    TableQuery[UserTable].forceInsert(employee),
    TableQuery[CourseTable].forceInsert(course),
    degreeService.tableQuery.forceInsertAll(degrees),
    TableQuery[LabworkTable].forceInsertAll(List(labwork, otherLabwork)),
    TableQuery[RoleTable].forceInsertAll(roles)
  )

  override protected val degreeService: DegreeDao = new DegreeDaoSpec()

  override protected val authorityService: AuthorityDao = {
    lazy val sharedDb = db

    new AuthorityDao {

      override protected def roleService: RoleDao = new RoleDao {

        override protected def db: driver.PostgresProfile.backend.Database = sharedDb
      }

      override protected def db: PostgresProfile.backend.Database = sharedDb
    }
  }

  override protected val labworkApplicationService: LabworkApplicationDao = {
    lazy val sharedDb = db

    new LabworkApplicationDaoSpec {
      override lazy val db = sharedDb // TODO STILL NEEDED?
    }
  }

  override protected def name: String = "user"

  override protected val dbEntity: UserDb = UserDb("delete", "delete", "delete", "delete", User.StudentType, Some("regId"), Some(degrees.head.id))

  override protected val dbEntities: List[UserDb] = dbUser

  override protected val invalidDuplicateOfDbEntity: UserDb = UserDb(dbEntity.systemId, "delete2", "delete2", "delete2", User.StudentType, None, None, lastModified, None, dbEntity.id)

  override protected val invalidUpdateOfDbEntity: UserDb = UserDb("new SystemId", "new lastname", dbEntity.firstname, dbEntity.email, dbEntity.status, None, None, lastModified, None, dbEntity.id)

  override protected val validUpdateOnDbEntity: UserDb = UserDb(dbEntity.systemId, "new lastname", dbEntity.firstname, dbEntity.email, dbEntity.status, None, None, lastModified, None, dbEntity.id)

  override protected val lwmEntity: User = dbEntity.toUniqueEntity

  override protected val lwmAtom: User = StudentAtom(
    dbEntity.systemId,
    dbEntity.lastname,
    dbEntity.firstname,
    dbEntity.email,
    dbEntity.registrationId.get,
    degrees.head.toUniqueEntity,
    dbEntity.id
  )
}*/
