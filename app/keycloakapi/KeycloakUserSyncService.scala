package keycloakapi

import dao.{DegreeDao, UserDao}
import models.{Employee, Lecturer, Student, User}
import play.api.libs.json.{JsError, JsResult, JsSuccess}
import utils.Ops.zipOpt

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class KeycloakUserSyncService @Inject() (
    private val apiService: KeycloakApiService,
    private val userDao: UserDao,
    private val degreeDao: DegreeDao,
    private implicit val ctx: ExecutionContext
) {

  def syncAllUsers() =
    for {
      currentUsers <- userDao.get(atomic = false)
      degrees <- degreeDao.get(atomic = false)
      users <- apiService.fetchUsers(currentUsers)(_.systemId)
    } yield update(users)(e =>
      degrees.find(_.abbreviation.toLowerCase == e.toLowerCase).map(_.id)
    )

  def update(
      users: Seq[JsResult[(User, KeycloakUser)]]
  )(enrollmentId: String => Option[UUID]): Seq[Either[String, User]] = {
    def findDegree(e: String): Either[String, UUID] =
      enrollmentId(e).toRight(s"no degree found for $e")

    users.map {
      case JsSuccess((existing, latest), _)
          if existing.systemId.toLowerCase == latest.systemId.toLowerCase =>
        existing match {
          case employee: Employee =>
            Right(
              employee.copy(
                lastname = latest.lastname,
                firstname = latest.firstname,
                email = latest.email
              )
            )
          case lecturer: Lecturer =>
            Right(
              lecturer.copy(
                lastname = latest.lastname,
                firstname = latest.firstname,
                email = latest.email
              )
            )
          case student: Student =>
            zipOpt(latest.registrationId, latest.degreeAbbrev)
              .toRight(
                "student must have a registration id and an enrollment"
              )
              .flatMap(a => findDegree(a._2).map(id => (a._1, id)))
              .map { case (reg, deg) =>
                student.copy(
                  lastname = latest.lastname,
                  firstname = latest.firstname,
                  email = latest.email,
                  registrationId = reg,
                  enrollment = deg
                )
              }
        }
      case JsSuccess((existing, latest), _) =>
        Left(s"user miss match: $existing and $latest")
      case JsError(errors) =>
        Left(errors.map(a => a._2.mkString(",")).mkString(","))
    }
  }
}
