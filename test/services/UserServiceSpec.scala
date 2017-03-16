package services

import java.util.UUID

import base.PostgresDbSpec
import models._

class UserServiceSpec extends PostgresDbSpec with UserService {
  import scala.util.Random.nextInt

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
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    import slick.driver.PostgresDriver.api._

    await(db.run(DBIO.seq(
      degreeTable.schema.create,
      tableQuery.schema.create,
      degreeTable.forceInsertAll(degrees),
      tableQuery.forceInsertAll(dbUser)
    )))
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    import slick.driver.PostgresDriver.api._

    await(db.run(DBIO.seq(
      tableQuery.schema.drop,
      degreeTable.schema.drop
    )))
  }
}