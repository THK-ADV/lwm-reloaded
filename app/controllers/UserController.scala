package controllers

import java.util.UUID

import controllers.UserController._
import controllers.crud.{Chunked, _}
import models.UriGenerator
import models.security.Permissions
import models.users.{Employee, Student, StudentAtom, User}
import modules.store.BaseNamespace
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Controller, Request, Result}
import services.{LdapService, RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.{Namespace, Resolvers, SesameRepository}
import utils.{Attempt, Continue, LwmMimeType, Return}
import utils.Ops.MonadInstances.optM

import scala.collection.Map
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object UserController {
  implicit val writes: Writes[User] = new Writes[User] {
    override def writes(user: User): JsValue = user match {
      case student: Student => Json.toJson(student)
      case employee: Employee => Json.toJson(employee)(Employee.writes)
    }
  }

  private def withFilter[A <: User](queryString: Map[String, Seq[String]])(all: Set[A]): Try[Set[A]] = {
    queryString.foldRight(Try(all)) {
      case ((`degreeAttribute`, degrees), users) => users flatMap { set =>
        Try(UUID.fromString(degrees.head)) map (degree => set filter {
          case Student(_, _, _, _, _, e, _, _) => e == degree
          case _ => false
        })
      }
      case ((`statusAttribute`, states), users) => users map { set =>
        states.foldLeft(set)((set, status) => set filter {
          case Employee(_, _, _, _, s, _, _) => s == status
          case _ => false
        }
        )
      }
      case ((`firstnameAttribute`, firstnames), users) => users map { set =>
        firstnames.foldLeft(set) { (set, firstname) =>
          set.filter(_.firstname.toLowerCase.contains(firstname.toLowerCase))
        }
      }
      case ((`lastnameAttribute`, lastnames), users) => users map { set =>
        lastnames.foldLeft(set) { (set, lastname) =>
          set.filter(_.lastname.toLowerCase.contains(lastname.toLowerCase))
        }
      }
      case ((`systemIdAttribute`, systemIds), users) => users map { set =>
        systemIds.foldLeft(set) { (set, id) =>
          set.filter(_.systemId.toLowerCase.contains(id.toLowerCase))
        }
      }
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  val degreeAttribute = "degree"
  val statusAttribute = "status"
  val systemIdAttribute = "systemId"
  val firstnameAttribute = "firstname"
  val lastnameAttribute = "lastname"
}

class UserController(val roleService: RoleService, val sessionService: SessionHandlingService, val repository: SesameRepository, val namespace: Namespace, val resolvers: Resolvers, val ldapService: LdapService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with Filterable[User]
  with ContentTyped
  with BaseNamespace
  with Chunked
  with Stored
  with Retrieved[User, User]
  with RdfSerialisation[User, User] {

  import Student.writesAtom
  import defaultBindings.{StudentDescriptor, StudentAtomDescriptor, EmployeeDescriptor}

  implicit val ns: Namespace = repository.namespace

  override implicit val mimeType: LwmMimeType = LwmMimeType.userV1Json

  override implicit val descriptor: Descriptor[Sesame, User] = defaultBindings.UserDescriptor

  override val descriptorAtom: Descriptor[Sesame, User] = descriptor

  override implicit val uriGenerator: UriGenerator[User] = User

  def coatomic(atom: StudentAtom): Student = Student(atom.systemId, atom.lastname, atom.firstname, atom.email, atom.registrationId, atom.enrollment.id, atom.invalidated, atom.id)

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(Permissions.user.get)
    case GetAll => PartialSecureBlock(Permissions.user.getAll)
    case Create => PartialSecureBlock(Permissions.prime)
    case _ => PartialSecureBlock(Permissions.god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[User]): Try[Set[User]] = withFilter(queryString)(all)

  def student(id: String) = contextFrom(Get) action { request =>
    val uri = s"$namespace${request.uri}".replace("students", "users")

    retrieve[Student](uri)
      .mapResult(s => Ok(Json.toJson(s)).as(mimeType))
  }

  def studentAtomic(id: String) = contextFrom(Get) action { request =>
    val uri = s"$namespace${request.uri}".replace("/atomic", "").replace("students", "users")

    retrieve[StudentAtom](uri)
      .mapResult(s => Ok(Json.toJson(s)).as(mimeType))
  }

  def employee(id: String) = contextFrom(Get) action { request =>
    val uri = s"$namespace${request.uri}".replace("employees", "users")

    retrieve[Employee](uri)
      .mapResult(e => Ok(Json.toJson(e)).as(mimeType))
  }

  def allEmployees() = contextFrom(GetAll) action { request =>
    retrieveAll[Employee]
      .flatMap(filtered(request))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allStudents() = contextFrom(GetAll) action { request =>
    retrieveAll[Student]
      .flatMap(filtered(request))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allAtomicStudents() = contextFrom(GetAll) action { request =>
    retrieveAll[StudentAtom]
      .flatMap { students =>
        val non = students map coatomic
        filtered(request)(non)
          .map(set => students filter (s => set exists (_.id == s.id)))
      }
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def user(id: String) = contextFrom(Get) action { request =>
    val uri = s"$namespace${request.uri}"

    retrieve[User](uri)
      .mapResult(u => Ok(Json.toJson(u)).as(mimeType))
  }

  def allUsers() = contextFrom(GetAll) action { request =>
    retrieveAll[User]
      .flatMap(filtered(request))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def userAtomic(id: String) = contextFrom(Get) action { request =>
    val uri = s"$namespace${request.uri}".replace("/atomic", "")

    retrieve[User](uri)
      .flatMap {
        case s: Student => retrieve[StudentAtom](User generateUri s) map (a => Json.toJson(a))
        case e: Employee => Continue(Json.toJson(e))
      }
      .mapResult(js => Ok(js).as(mimeType))
  }

  //TODO: This is shit
  def allUserAtomic() = contextFrom(GetAll) action { request =>
    retrieveAll[User]
      .flatMap(filtered(request))
      .flatMap { set =>
        set.foldLeft(Attempt(List.empty[JsValue])) {
          case (ret, s: Student) =>
            for {
              list <- ret
              elm <- retrieve[StudentAtom](User generateUri s) map (a => Json.toJson(a))
            } yield list.+:(elm)

          case (ret, e: Employee) => ret map (list => list.+:(Json.toJson(e)))
          case (ret, _) => ret
        }
      }
      .map(list => chunk(list.toSet))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def paired[A](request: Request[A], systemId: String): Attempt[Set[Student]] = {
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.NaturalTrasformations._

    val lwm = LWMPrefix[repository.Rdf]
    val currentUser = ((request.session(_)) andThen UUID.fromString andThen (User.generateUri(_)(namespace))) (SessionController.userId)

    val query = select("student") where {
      **(v("student"), p(lwm.systemId), o(systemId))
    }

    queried {
      repository.prepareQuery(query).
        select(_.get("student")).
        changeTo(_.headOption).
        transform(_.fold(Set.empty[String])(value => Set(value.stringValue(), currentUser))).
        requestAll(repository.getMany[Student](_))
    }
  }

  def buddy(systemId: String) = contextFrom(Get) action { request =>
    paired(request, systemId)
      .map { students =>
        students.find(_.systemId == systemId).fold(
          NotFound(Json.obj(
            "status" -> "KO",
            "message" -> "No such element..."
          ))
        ) {
          student =>
            if (students.groupBy(_.enrollment).size == 1)
              Ok(Json.obj(
                "status" -> "OK",
                "id" -> student.id
              ))
            else
              BadRequest(Json.obj(
                "status" -> "KO",
                "message" -> "Students are not part of the same degree"
              ))
        }
      }
      .mapResult(identity)
  }

  // TODO expand future to attempt
  def createOrUpdate(systemId: String) = contextFrom(Create) asyncContentTypedAction { implicit request =>
    import scala.concurrent.ExecutionContext.Implicits.global

    def ldap(f: User => Try[Result]): Future[Result] = {
      (for {
        ldapUser <- ldapService.user(systemId)(resolvers.degree)
        result <- Future.fromTry(f(ldapUser))
      } yield result) recover {
        case NonFatal(t) =>
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> t.getMessage
          ))
      }
    }

    resolvers.userId(systemId) match {
      case Success(Some(uuid)) =>
        def adapt(user: User, id: UUID): User = user match {
          case s: Student => Student(s.systemId, s.lastname, s.firstname, s.email, s.registrationId, s.enrollment, s.invalidated, id)
          case e: Employee => Employee(e.systemId, e.lastname, e.firstname, e.email, e.status, e.invalidated, id)
        }

        ldap { user =>
          val u = adapt(user, uuid)
          repository.update(u).map(_ => Ok(Json.toJson(u)).as(mimeType))
        }

      case Success(None) =>
        ldap (user => resolvers.missingUserData(user).map(_ => Created(Json.toJson(user)).as(mimeType)))

      case Failure(e) =>
        Future.successful(InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def filtered[R, X <: User](request: Request[R])(users: Set[X]): Attempt[Set[X]] = {
    withFilter(request.queryString)(users) match {
      case Success(set) => Continue(set)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }
}
