package services

import java.util.UUID

import base.PostgresDbSpec
import models._
import slick.dbio.Effect.Write

class UserServiceSpec extends PostgresDbSpec with UserService {
  import scala.util.Random.nextInt
  import slick.driver.PostgresDriver.api._

  val maxUser = 100
  val maxDegrees = 5

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

      DbUser(i.toString, i.toString, i.toString, i.toString, status, regId, enrollment, None, UUID.randomUUID)
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
      /*val ldapUser = LdapUser("systemId", "firstname", "lastname", "email", User.EmployeeType, None, None)

      val (user, maybeAuth) = await(createOrUpdate(ldapUser))
      val allUser = await(get())

      user.toDbUser shouldBe ldapUser.toDbUser
      allUser.size shouldBe dbUser.size + 1
      maybeAuth shouldBe defined*/
    }
  }

  override protected def fillDb: DBIOAction[Unit, NoStream, Write] = DBIO.seq(degreeService.tableQuery.forceInsertAll(degrees), tableQuery.forceInsertAll(dbUser))

  override protected val degreeService: DegreeService = new DegreeServiceSpec()

  override protected val authorityService: AuthorityService = new AuthorityServiceSpec()
}