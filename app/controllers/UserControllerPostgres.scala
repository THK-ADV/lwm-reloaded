package controllers

import models._
import play.api.libs.json.{JsError, JsValue, Json, Reads}
import play.api.mvc.{Controller, Request, Result}
import services._
import store.{Resolvers, TableFilter, UserTable}
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object UserControllerPostgres {
  lazy val statusAttribute = "status"
  lazy val degreeAttribute = "degree"
  lazy val systemIdAttribute = "systemId"
  lazy val firstnameAttribute = "firstname"
  lazy val lastnameAttribute = "lastname"
}

class UserControllerPostgres(val roleService: RoleService, val sessionService: SessionHandlingService, val resolvers: Resolvers, val ldapService: LdapService, val userService: UserService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with ContentTyped
  with Chunked
  with PostgresResult {

  import scala.concurrent.ExecutionContext.Implicits.global

  override implicit def mimeType = LwmMimeType.userV1Json
  import models.User.{writes, reads, UserProtocol}

  def allUsers() = contextFrom(GetAll) asyncAction { request =>
    import controllers.UserControllerPostgres._

    val (queryString, atomic) = extractAtomic(request.queryString)

    val userFilter = queryString.foldLeft(Try(List.empty[TableFilter[UserTable]])) {
      case (list, (`statusAttribute`, status)) => list.map(_.+:(UserStatusFilter(status.head)))
      case (list, (`systemIdAttribute`, systemId)) => list.map(_.+:(UserSystemIdFilter(systemId.head)))
      case (list, (`lastnameAttribute`, lastname)) => list.map(_.+:(UserLastnameFilter(lastname.head)))
      case (list, (`firstnameAttribute`, firstname)) => list.map(_.+:(UserFirstnameFilter(firstname.head)))
      case (list, (`degreeAttribute`, degree)) => list.map(_.+:(UserDegreeFilter(degree.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }

    (for {
      filter <- Future.fromTry(userFilter)
      users <- userService.get(filter, atomic)
    } yield users).jsonResult
  }

  def user(id: String) = contextFrom(Get) asyncAction { request =>
    val atomic = extractAtomic(request.queryString)._2

    userService.get(List(UserIdFilter(id)), atomic).map(_.headOption).jsonResult(id)
  }

  private def parse[A](request: Request[JsValue])(implicit reads: Reads[A]): Try[A] = {
    request.body.validate[A].fold[Try[A]](
      errors => Failure(new Throwable(JsError.toJson(errors).toString())),
      success => Success(success)
    )
  }

  def createOrUpdate() = contextFrom(Create) asyncContentTypedAction { request =>
    import models.User.writes
    import models.PostgresAuthority.writesAtom

    val result = for {
      systemId <- Future.fromTry(parse[UserProtocol](request).map(p => UserSystemIdFilter(p.systemId)))
      ldapUser <- ldapService.user2(systemId.value)
      degrees <- DegreeService.get()
      existing <- userService.get(List(systemId))
      maybeEnrollment = ldapUser.degreeAbbrev.flatMap(abbrev => degrees.find(_.abbreviation.toLowerCase == abbrev.toLowerCase)).map(_.id)
      dbUser = DbUser(ldapUser.systemId, ldapUser.lastname, ldapUser.firstname, ldapUser.email, ldapUser.status, ldapUser.registrationId, maybeEnrollment, existing.headOption.fold(ldapUser.id)(_.id))
      updated <- userService.createOrUpdate(dbUser)
      maybeAuth <- updated.fold[Future[Option[PostgresAuthorityAtom]]](Future.successful(None))(user => AuthorityService.createWith(user).mapTo[Option[PostgresAuthorityAtom]])
    } yield (dbUser, maybeAuth)

    result.map {
      case ((dbUser, maybeAuth)) =>
        val userJson = Json.toJson(dbUser.user)
        maybeAuth.fold {
          Ok(userJson)
        } { auth =>
          Created(Json.obj(
            "user" -> userJson,
            "authority" -> Json.toJson(auth)
          ))
        }
    } recover {
      case NonFatal(e) => internalServerError(e)
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(Permissions.user.get)
    case GetAll => PartialSecureBlock(Permissions.user.getAll)
    case Create => PartialSecureBlock(Permissions.prime)
    case _ => PartialSecureBlock(Permissions.god)
  }
}
