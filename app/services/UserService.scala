package services

import models._
import store.{DegreeTable, UserTable}

import scala.concurrent.Future

trait UserService extends AbstractDao[UserTable, DbUser] {

  import slick.driver.PostgresDriver.api._
  import scala.concurrent.ExecutionContext.Implicits.global

  private lazy val degrees = TableQuery[DegreeTable]

  def filter(status: String): Future[Seq[User]] = filter(_.status === status).map(_.map(_.user))

  def atomic(status: String): Future[Seq[PostgresStudentAtom]] = {
    val query = for {
      u <- tableQuery.filter(_.status === status)
      d <- degrees if u.enrollment === d.id
    } yield (u, d)

    db.run(query.result) map { users =>
      users.map {
        case ((s, d)) => PostgresStudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId.head, d, s.id)
      }
    }
  }

  override protected def tableQuery: TableQuery[UserTable] = TableQuery[UserTable]
}

object UserService extends UserService