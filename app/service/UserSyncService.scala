package service

import dao.{DegreeDao, UserDao}
import database.UserDb
import database.helper.LdapUserStatus._
import keycloakapi.{KeycloakApiService, KeycloakUser}
import models.{Employee, Lecturer, Student, User}
import play.api.libs.json.{JsError, JsResult, JsSuccess}
import utils.Ops.zipOpt

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UserSyncService @Inject() (
    private val apiService: KeycloakApiService,
    private val userDao: UserDao,
    private val degreeDao: DegreeDao,
    private implicit val ctx: ExecutionContext
) {

  case class Updated[A](previous: A, updated: A)

  def fetchUpdatedUser(user: User) =
    for {
      degrees <- degreeDao.get(atomic = false)
      (existing, res) <- apiService.fetchUser(user)(_.systemId)
      updatedUser <- update(existing, res)(e =>
        degrees.find(_.abbreviation.toLowerCase == e.toLowerCase).map(_.id)
      ).fold(
        err => Future.failed(new Throwable(err)),
        Future.successful
      )
    } yield Updated(user, updatedUser)

  def fetchAndUpdateUser(user: User) =
    for {
      user <- fetchUpdatedUser(user)
      res <- userDao.update(toUserDb(user.updated))
    } yield user.copy(updated = res.toUniqueEntity)

  def update(
      existing: User,
      res: JsResult[KeycloakUser]
  )(enrollmentId: String => Option[UUID]): Either[String, User] = {
    def findDegree(e: String): Either[String, UUID] =
      enrollmentId(e).toRight(s"no degree found for $e")

    res match {
      case JsSuccess(latest, _)
          if existing.systemId.toLowerCase == latest.systemId.toLowerCase =>
        existing match {
          case employee: Employee =>
            Right(updateEmployee(latest, employee))
          case lecturer: Lecturer =>
            Right(updateLecturer(latest, lecturer))
          case student: Student =>
            zipOpt(latest.registrationId, latest.degreeAbbrev)
              .toRight(
                "student must have a registration id and an enrollment"
              )
              .flatMap(a => findDegree(a._2).map(id => (a._1, id)))
              .map { case (reg, deg) =>
                updateStudent(latest, student, reg, deg)
              }
        }
      case JsSuccess(latest, _) =>
        Left(s"user miss match: $existing and $latest")
      case JsError(errors) =>
        val errs = errors.map(a => a._2.mkString(",")).mkString(",")
        Left(s"failed to update $existing, because: $errs")
    }
  }

  private def updateStudent(
      latest: KeycloakUser,
      student: Student,
      registrationId: String,
      degree: UUID
  ) = {
    student.copy(
      systemId = latest.systemId,
      campusId = latest.campusId,
      lastname = latest.lastname,
      firstname = latest.firstname,
      email = latest.email,
      registrationId = registrationId,
      enrollment = degree
    )
  }

  private def updateLecturer(latest: KeycloakUser, lecturer: Lecturer) =
    lecturer.copy(
      systemId = latest.systemId,
      campusId = latest.campusId,
      lastname = latest.lastname,
      firstname = latest.firstname,
      email = latest.email
    )

  private def updateEmployee(latest: KeycloakUser, employee: Employee) =
    employee.copy(
      systemId = latest.systemId,
      campusId = latest.campusId,
      lastname = latest.lastname,
      firstname = latest.firstname,
      email = latest.email
    )

  private def toUserDb(user: User): UserDb =
    user match {
      case Employee(systemId, campusId, lastname, firstname, email, id) =>
        UserDb(
          systemId,
          campusId,
          lastname,
          firstname,
          email,
          EmployeeStatus,
          None,
          None,
          id = id
        )
      case Lecturer(systemId, campusId, lastname, firstname, email, id) =>
        UserDb(
          systemId,
          campusId,
          lastname,
          firstname,
          email,
          LecturerStatus,
          None,
          None,
          id = id
        )
      case Student(
            systemId,
            campusId,
            lastname,
            firstname,
            email,
            registrationId,
            enrollment,
            id
          ) =>
        UserDb(
          systemId,
          campusId,
          lastname,
          firstname,
          email,
          StudentStatus,
          Some(registrationId),
          Some(enrollment),
          id = id
        )
    }
}
