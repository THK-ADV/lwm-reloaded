package services

import java.util.UUID

import models._
import slick.dbio.Effect.Write
import store.UserTable

final class UserServiceSpec extends AbstractDaoSpec[UserTable, DbUser, User] with UserService {
  import scala.util.Random.nextInt
  import slick.driver.PostgresDriver.api._

  val maxUser = 150
  val maxDegrees = 3

  val degrees: List[DegreeDb] = {
    (0 until maxDegrees).map(i => DegreeDb(i.toString, i.toString)).toList
  }

  val dbUser: List[DbUser] = {
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

      DbUser(i.toString, i.toString, i.toString, i.toString, status, regId, enrollment)
    }.toList
  }

  "A UserServiceSpec " should {
    "return a user by id (either atomic or non atomic)" in {
      val student = dbUser.find(_.status == User.StudentType)
      val studentAtom = student.map { u =>
        val d = u.enrollment.flatMap(id => degrees.find(_.id == id)).get.toDegree
        PostgresStudentAtom(u.systemId, u.lastname, u.firstname, u.email, u.registrationId.get, d, u.id)
      }

      val employee = dbUser.find(_.status == User.EmployeeType)
      val employeeAtom = employee.map(e => PostgresEmployee(e.systemId, e.lastname, e.firstname, e.email, e.id))

      await(get(List(UserIdFilter(student.get.id.toString)), atomic = false)).headOption shouldBe student.map(_.toUser)
      await(get(List(UserIdFilter(employee.get.id.toString)), atomic = false)).headOption shouldBe employee.map(_.toUser)
      await(get(List(UserIdFilter(studentAtom.get.id.toString)))).headOption shouldBe studentAtom
      await(get(List(UserIdFilter(employeeAtom.get.id.toString)))).headOption shouldBe employeeAtom
    }

    "filter users by certain attributes" in {
      val degree = degrees(nextInt(maxDegrees))
      val possibleUsers1 = dbUser.filter(u => u.status == User.StudentType && u.enrollment.get == degree.id).map(_.toUser)
      val possibleUsers2 = dbUser.filter(u => u.firstname.contains("5") && u.lastname.contains("5")).map(_.toUser)
      val possibleUsers3 = dbUser.filter(_.systemId == "10").map(_.toUser)

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
      val dbVariation = DbUser(ldapUser.systemId, ldapUser.lastname, ldapUser.firstname, ldapUser.email, ldapUser.status, None, None, lastModified, None, ldapUser.id)
      val dbVariation2 = DbUser(ldapUser2.systemId, ldapUser2.lastname, ldapUser2.firstname, ldapUser2.email, ldapUser2.status, ldapUser2.registrationId, Some(degree.id), lastModified, None, ldapUser2.id)

      val (user, maybeAuth) = await(createOrUpdate(ldapUser))
      val (user2, maybeAuth2) = await(createOrUpdate(ldapUser2))
      val allUser = await(get())
      val allAuths = await(authorityService.get())
      val maybeAuths = List(maybeAuth, maybeAuth2)

      toDbUser(user) shouldBe dbVariation
      toDbUser(user2) shouldBe dbVariation2
      allUser.size shouldBe dbUser.size + 2
      maybeAuths.forall(_.isDefined) shouldBe true
      maybeAuths.map(_.get).forall(a => allAuths.contains(a)) shouldBe true
    }

    "update a user from ldap when already exists" in {
      val chosenEmployee = dbUser.find(_.status == User.EmployeeType).get
      val chosenStudent = dbUser.find(_.status == User.StudentType).get
      val degree = chosenStudent.enrollment.flatMap(current => degrees.find(_.id == current))

      val ldapEmployee = LdapUser(chosenEmployee.systemId, "updateFirst", "updateLast", chosenEmployee.email, chosenEmployee.status, None, None)
      val dbEmployee = DbUser(ldapEmployee.systemId, ldapEmployee.lastname, ldapEmployee.firstname, ldapEmployee.email, ldapEmployee.status, None, None, lastModified, None, chosenEmployee.id)
      val ldapStudent = LdapUser(chosenStudent.systemId, "updateFirst2", chosenStudent.lastname, "updateEmail", chosenStudent.status, chosenStudent.registrationId, degree.map(_.abbreviation))
      val dbStudent = DbUser(ldapStudent.systemId, ldapStudent.lastname, ldapStudent.firstname, ldapStudent.email, ldapStudent.status, ldapStudent.registrationId, degree.map(_.id), lastModified, None, chosenStudent.id)

      val (employee, maybeAuth) = await(createOrUpdate(ldapEmployee))
      val (student, maybeAuth2) = await(createOrUpdate(ldapStudent))

      toDbUser(employee) shouldBe dbEmployee
      toDbUser(student) shouldBe dbStudent
      List(maybeAuth, maybeAuth2).forall(_.isEmpty) shouldBe true
    }

    "deny buddy requests properly" in {
      val chosenDegree = degrees.last
      val labwork = LabworkDb("label", "desc", UUID.randomUUID, UUID.randomUUID, chosenDegree.id)
      val student = dbUser.find(u => u.status == User.StudentType && u.enrollment.contains(chosenDegree.id)).get
      val buddy = dbUser.find(u => u.status == User.StudentType && !u.enrollment.contains(chosenDegree.id)).get

      await(buddyResult(student.id.toString, "not in system", labwork.id.toString)) shouldBe NotExisting
      await(buddyResult(student.id.toString, buddy.systemId, labwork.id.toString)) shouldBe Denied
    }

    "allow a buddy request" in {
      val chosenDegree = degrees.last
      val labwork = LabworkDb("label", "desc", UUID.randomUUID, UUID.randomUUID, chosenDegree.id)
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
      await(buddyResult(student.id.toString, buddy.systemId, labwork.id.toString)) shouldBe Allowed
    }

    "almost allow a buddy request" in {
      val chosenDegree = degrees.last
      val labwork = LabworkDb("label", "desc", UUID.randomUUID, UUID.randomUUID, chosenDegree.id)
      val otherLabwork = LabworkDb("label2", "desc2", UUID.randomUUID, UUID.randomUUID, chosenDegree.id)
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
      await(buddyResult(student.id.toString, buddy.systemId, labwork.id.toString)) shouldBe Almost
    }
  }

  private def toDbUser(user: User) = user match {
    case s: PostgresStudent =>
      DbUser(s.systemId, s.lastname, s.firstname, s.email, User.StudentType, Some(s.registrationId), Some(s.enrollment), lastModified, None, s.id)
    case e: PostgresEmployee =>
      DbUser(e.systemId, e.lastname, e.firstname, e.email, User.EmployeeType, None, None, lastModified, None, e.id)
    case l: PostgresLecturer =>
      DbUser(l.systemId, l.lastname, l.firstname, l.email, User.LecturerType, None, None, lastModified, None, l.id)
  }

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    degreeService.tableQuery.forceInsertAll(degrees)
  )

  override protected val degreeService: DegreeService = new DegreeServiceSpec()

  override protected val authorityService: AuthorityService = new AuthorityServiceSpec()

  override protected val labworkApplicationService: LabworkApplicationService2 = new LabworkApplicationService2Spec()

  override protected def name: String = "user"

  override protected val dbEntity: DbUser = DbUser("delete", "delete", "delete", "delete", User.StudentType, Some("regId"), Some(degrees.head.id))

  override protected val dbEntities: List[DbUser] = dbUser

  override protected val invalidDuplicateOfDbEntity: DbUser = DbUser(dbEntity.systemId, "delete2", "delete2", "delete2", User.StudentType, None, None, lastModified, None, dbEntity.id)

  override protected val invalidUpdateOfDbEntity: DbUser = DbUser("new SystemId", "new lastname", dbEntity.firstname, dbEntity.email, dbEntity.status, None, None, lastModified, None, dbEntity.id)

  override protected val validUpdateOnDbEntity: DbUser = DbUser(dbEntity.systemId, "new lastname", dbEntity.firstname, dbEntity.email, dbEntity.status, None, None, lastModified, None, dbEntity.id)

  override protected val lwmEntity: User = dbEntity.toUser

  override protected val lwmAtom: User = PostgresStudentAtom(
    dbEntity.systemId,
    dbEntity.lastname,
    dbEntity.firstname,
    dbEntity.email,
    dbEntity.registrationId.get,
    degrees.head.toDegree,
    dbEntity.id
  )
}