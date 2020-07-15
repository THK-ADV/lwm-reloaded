package utils

import base.PostgresDbSpec
import dao.AbstractDaoSpec.{degrees, populateEmployees, populateStudents}
import dao.UserDao
import database.{DegreeTable, UserTable}
import play.api.inject.guice.GuiceableModule
import utils.student_query_engine.StudentQueryEngine

class StudentQueryEngineSpec extends PostgresDbSpec {

  private val engine = app.injector.instanceOf(classOf[StudentQueryEngine])
  private val userDao = app.injector.instanceOf(classOf[UserDao])

  import profile.api._
  import utils.student_query_engine.Expression._
  import utils.student_query_engine.Operator._

  override protected def dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(populateEmployees(10) ::: populateStudents(20)(degrees))
  )

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  "A StudentQueryEngineSpec" should {
    "filter students by a given lastname" in {
      async(
        userDao.filter(engine.makeFilter(Single(Key.Lastname, "5")), atomic = false)
      ) { users =>
        users.size shouldBe 3
        users.forall(_.lastname.contains("5")) shouldBe true
      }
      async(
        userDao.filter(engine.makeFilter(Single(Key.Lastname, "3")), atomic = false)
      ) { users =>
        users.size shouldBe 1 + 2
        users.forall(_.lastname.contains("3")) shouldBe true
      }
      async(
        userDao.filter(engine.makeFilter(Single(Key.Lastname, "zz")), atomic = false)
      ) { users =>
        users.isEmpty shouldBe true
      }
      async(
        userDao.filter(engine.makeFilter(Combined(Single(Key.Lastname, "1"), Single(Key.Lastname, "5"), And)), atomic = false)
      ) { users =>
        users.size shouldBe 1
        users.head.lastname shouldBe "15"
      }
      async(
        userDao.filter(engine.makeFilter(Combined(Single(Key.Lastname, "3"), Single(Key.Lastname, "5"), Or)), atomic = false)
      ) { users =>
        users.size shouldBe 3 + 1 + 2
        users.forall(u => u.lastname.contains("3") || u.lastname.contains("5")) shouldBe true
      }
      async(
        userDao.filter(engine.makeFilter(Combined(Single(Key.Lastname, "3"), Single(Key.Lastname, "5"), And)), atomic = false)
      ) { users =>
        users.isEmpty shouldBe true
      }
    }
  }
}
