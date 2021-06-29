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

  import utils.Ops.unwrap

  case class Updated[A](previous: A, updated: A)

  def fetchUpdatedUser(id: UUID) =
    for {
      currentUser <- unwrap(
        userDao.getSingle(id, atomic = false),
        () => s"user not found with id: $id"
      )
      degrees <- degreeDao.get(atomic = false)
      (existing, res) <- apiService.fetchUser(currentUser)(_.systemId)
      updatedUser <- update(existing, res)(e =>
        degrees.find(_.abbreviation.toLowerCase == e.toLowerCase).map(_.id)
      ).fold(
        err => Future.failed(new Throwable(err)),
        Future.successful
      )
    } yield Updated(currentUser, updatedUser)

  def fetchAndUpdateUser(id: UUID) =
    for {
      user <- fetchUpdatedUser(id)
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
      case JsSuccess(latest, _) =>
        Left(s"user miss match: $existing and $latest")
      case JsError(errors) =>
        val errs = errors.map(a => a._2.mkString(",")).mkString(",")
        Left(s"failed to update $existing, because: $errs")
    }
  }

  private def partitionMap[L, R1, R2](
      either: Seq[Either[L, R1]]
  )(f: R1 => R2): (List[L], List[R2]) =
    either.foldLeft((List.empty[L], List.empty[R2])) { case (acc, e) =>
      e match {
        case Left(l) =>
          (l :: acc._1, acc._2)
        case Right(r) =>
          (acc._1, f(r) :: acc._2)
      }
    }

  private def toUserDb(user: User): UserDb =
    user match {
      case Employee(systemId, lastname, firstname, email, id) =>
        UserDb(
          systemId,
          lastname,
          firstname,
          email,
          EmployeeStatus,
          None,
          None,
          id = id
        )
      case Lecturer(systemId, lastname, firstname, email, id) =>
        UserDb(
          systemId,
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
            lastname,
            firstname,
            email,
            registrationId,
            enrollment,
            id
          ) =>
        UserDb(
          systemId,
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
