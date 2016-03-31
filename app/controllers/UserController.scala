package controllers

import java.util.UUID

import controllers.UserController._
import controllers.crud._
import models.Degree
import models.security.Permissions
import models.users.{Employee, Student, StudentAtom, User}
import modules.store.BaseNamespace
import play.api.libs.json._
import play.api.mvc.{AnyContent, Controller, Request, Result}
import services.{RoleService, SessionHandlingService}
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.Ops.MonadInstances.{optM, tryM}
import utils.Ops.TraverseInstances._
import utils.Ops._

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object UserController {
  def manyToJson[A <: User](repo: SesameRepository)(output: Set[A], f: A => Try[Option[JsValue]]): Try[JsValue] =
    output.foldLeft(Try(Option(JsArray()))) { (t, user) => t.bipeek(f(user))(_ :+ _) } map {
    case Some(jsarr) => jsarr
    case None => JsArray()
  }

  def toJson(user: User): JsValue = user match {
    case student: Student => Json.toJson(student)
    case employee: Employee => Json.toJson(employee)
  }

  private def withFilter[A <: User](queryString: Map[String, Seq[String]])(all: Set[A]): Try[Set[A]] = {
    queryString.foldRight(Try(all)) {
      case ((`degreeAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(d => bifilter(set)(_.enrollment == d)(_ => false)))
      case ((`statusAttribute`, v), t) => t map (set => v.foldLeft(set)((set, status) => bifilter(set)(_ => false)(_.status == status))) //replace with status
      case ((_, _), _) => Failure(new Throwable("Unknown attribute"))
    }
  }

  private def bifilter[A <: User](s: Set[A])(f: Student => Boolean)(g: Employee => Boolean): Set[A] = s filter {
    case s: Student => f(s)
    case e: Employee => g(e)
  }

  lazy val degreeAttribute = "degree"
  lazy val statusAttribute = "status"
}

class UserController(val roleService: RoleService, val sessionService: SessionHandlingService, val repository: SesameRepository, val namespace: Namespace) extends
  Controller with
  Secured with
  SessionChecking with
  SecureControllerContext with
  Filterable[User] with
  ContentTyped with
  BaseNamespace with
  Atomic[User] {

  val bindings = Bindings[repository.Rdf](namespace)

  import bindings.UserBinding
  import bindings.StudentBinding
  import bindings.EmployeeBinding

  def student(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}".replace("students", "users")
    repository.get[Student](uri)(StudentBinding.studentBinder)
  } (a => Ok(Json.toJson(a)).as(mimeType))

  def studentAtomic(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}".replace("/atomic", "").replace("students", "users")
    repository.get[Student](uri)(StudentBinding.studentBinder) flatPeek atomize
  } (Ok(_).as(mimeType))

  def employee(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}".replace("employees", "users")
    repository.get[Employee](uri)(EmployeeBinding.employeeBinder)
  } (a => Ok(Json.toJson(a)).as(mimeType))

  def allEmployees(secureContext: SecureContext = contextFrom(GetAll)) = many(secureContext) { request =>
    repository.get[Employee](EmployeeBinding.employeeBinder, EmployeeBinding.classUri)
  } (a => Ok(Json.toJson(a)).as(mimeType))

  def allStudents(secureContext: SecureContext = contextFrom(GetAll)) = many(secureContext) { request =>
    repository.get[Student](StudentBinding.studentBinder, StudentBinding.classUri)
  } (a => Ok(Json.toJson(a)).as(mimeType))

  def allAtomicStudents(secureContext: SecureContext = contextFrom(GetAll)) = gets(secureContext) { request =>
    repository.get[Student](StudentBinding.studentBinder, StudentBinding.classUri) map {
      case set if set.nonEmpty =>
        if(request.queryString.isEmpty) handle(gatomize(set))(InternalServerError(_))
        else {
          val makeResult =
            (withFilter(request.queryString)(_: Set[Student])) andThen (_.flatMap(gatomize)) andThen (handle(_)(ServiceUnavailable(_)))
          makeResult(set)
        }
      case set =>
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
    }
  }

  def get(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}"
    repository.get[User](uri)(UserBinding.userBinder)
  } (a => Ok(toJson(a)).as(mimeType))

  def all(secureContext: SecureContext = contextFrom(GetAll)) = {
    val makeResult = (jsonify(_: Set[User])(user => Try(Option(toJson(user))))) andThen (handle(_)(InternalServerError(_)))

    many(secureContext) { request =>
      repository.get[User](UserBinding.userBinder, UserBinding.classUri)

    } (makeResult)
  }

  def getAtomic(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}".replace("/atomic", "")
    repository.get[User](uri)(UserBinding.userBinder).flatPeek(atomize)
  } (Ok(_).as(mimeType))

  def allAtomic(secureContext: SecureContext = contextFrom(GetAll)) = gets(secureContext) { request =>
    repository.get[User](UserBinding.userBinder, UserBinding.classUri) map { set =>
        if(request.queryString.isEmpty) handle(atomizeMany(set))(InternalServerError(_))
        else {
          val makeResult =
            (getWithFilter(request.queryString)(_)) andThen (_.flatMap(atomizeMany)) andThen (handle(_)(ServiceUnavailable(_)))

          makeResult(set)
        }
    }
  }

    private def handle(t: Try[JsValue])(failure: JsObject => Result): Result = t match {
      case Success(json) =>
        Ok(json).as(mimeType)
      case Failure(e) =>
        failure(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }

  private def gets(secureContext: SecureContext)(f: Request[AnyContent] => Try[Result]) = secureContext action { request =>
    f(request) match {
      case Success(result) => result
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  private def one[A](secureContext: SecureContext)(f: Request[AnyContent] => Try[Option[A]])(g: A => Result) = gets(secureContext) { request =>
    f(request) map {
      case Some(a) => g(a)
      case None =>
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
    }
  }

  private def many[A <: User](secureContext: SecureContext)(f: Request[AnyContent] => Try[Set[A]])(g: Set[A] => Result) = gets(secureContext) { request =>
    f(request) map { set =>
        if(request.queryString.isEmpty) g(set)
        else withFilter(request.queryString)(set) match {
          case Success(filtered) => g(filtered)
          case Failure(e) =>
            ServiceUnavailable(Json.obj(
              "status" -> "KO",
              "message" -> e.getMessage
            ))
        }
    }
  }
  private def jsonify[A <: User](set: Set[A])(f: A => Try[Option[JsValue]]) = manyToJson(repository)(set, f)

  private def gatomize[A <: User](output: Set[A]): Try[JsValue] = jsonify(output)(atomize)

  override implicit val mimeType: LwmMimeType = LwmMimeType.userV1Json

  override protected def atomize(output: User): Try[Option[JsValue]] = {
    import bindings.DegreeBinding._
    import models.users.Student.atomicWrites

    output match {
      case s@Student(_, _, _, _, registrationId, enrollment, _) =>
        repository.get[Degree](Degree.generateUri(enrollment)(namespace)).peek { degree =>
          val atom = StudentAtom(output.systemId, output.lastname, output.firstname, output.email, registrationId, degree, output.id)
          Json.toJson(atom)
        }(tryM, optM)

      case employee: Employee => Success(Some(Json.toJson(employee)))

    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(Permissions.user.get)
    case GetAll => PartialSecureBlock(Permissions.user.getAll)
    case _ => PartialSecureBlock(Permissions.god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[User]): Try[Set[User]] = withFilter(queryString)(all)
}
