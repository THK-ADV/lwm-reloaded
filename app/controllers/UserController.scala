package controllers

import java.util.UUID

import controllers.UserController._
import controllers.crud.{Chunkable, _}
import models.Degree
import models.security.Permissions
import models.users.{Employee, Student, StudentAtom, User}
import modules.store.BaseNamespace
import play.api.libs.json._
import play.api.mvc.{AnyContent, Controller, Request, Result}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.Ops.MonadInstances.{optM, tryM}
import utils.Ops.TraverseInstances._
import utils.Ops._

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object UserController {

  implicit val writes: Writes[User] = new Writes[User] {
    override def writes(user: User): JsValue = user match {
      case student: Student => Json.toJson(student)
      case employee: Employee => Json.toJson(employee)
    }
  }

  private def withFilter[A <: User](queryString: Map[String, Seq[String]])(all: Set[A]): Try[Set[A]] = {
    queryString.foldRight(Try(all)) {
      case ((`degreeAttribute`, degrees), users) => users flatMap { set =>
        Try(UUID.fromString(degrees.head)) map (degree => set filter {
          case Student(_, _, _, _, _, e, _) => e == degree
          case _ => false
        })
      }
      case ((`statusAttribute`, states), users) => users map { set =>
        states.foldLeft(set)((set, status) => set filter {
          case Employee(_, _, _, _, s, _) => s == status
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

class UserController(val roleService: RoleService, val sessionService: SessionHandlingService, val repository: SesameRepository, val namespace: Namespace) extends
  Controller with
    Secured with
    SessionChecking with
    SecureControllerContext with
    Filterable[User] with
    ContentTyped with
    BaseNamespace with
    Atomic[User] with
    Chunkable[User] { self =>

  val studentChunker = new Chunkable[Student] with Atomic[Student] {
    override protected def atomize(output: Student): Try[Option[JsValue]] = self.atomize(output)
  }

  val employeeChunker = new Chunkable[Employee] with Atomic[Employee] {
    override protected def atomize(output: Employee): Try[Option[JsValue]] = self.atomize(output)
  }

  val bindings = Bindings[repository.Rdf](namespace)

  import bindings.UserBinding
  import bindings.StudentBinding
  import bindings.EmployeeBinding

  def student(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}".replace("students", "users")
    repository.get[Student](uri)(StudentBinding.studentBinder)
  } { student =>
    Ok(Json.toJson(student)).as(mimeType)
  }

  def studentAtomic(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}".replace("/atomic", "").replace("students", "users")
    repository.get[Student](uri)(StudentBinding.studentBinder) flatPeek atomize
  } { json =>
    Ok(json).as(mimeType)
  }

  def employee(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}".replace("employees", "users")
    repository.get[Employee](uri)(EmployeeBinding.employeeBinder)
  } { employee =>
    Ok(Json.toJson(employee)).as(mimeType)
  }

  def allEmployees(secureContext: SecureContext = contextFrom(GetAll)) = many(secureContext) { request =>
    repository.get[Employee](EmployeeBinding.employeeBinder, EmployeeBinding.classUri)
  } { employees =>
    Ok.chunked(employeeChunker.chunkSimple(employees)).as(mimeType)
  }

  def allStudents(secureContext: SecureContext = contextFrom(GetAll)) = many(secureContext) { request =>
    repository.get[Student](StudentBinding.studentBinder, StudentBinding.classUri)
  } { students =>
    Ok.chunked(studentChunker.chunkSimple(students)).as(mimeType)
  }

  def allAtomicStudents(secureContext: SecureContext = contextFrom(GetAll)) = many(secureContext) { request =>
    repository.get[Student](StudentBinding.studentBinder, StudentBinding.classUri)
  } { students =>
    Ok.chunked(studentChunker.chunkAtoms(students)).as(mimeType)
  }

  def user(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}"
    repository.get[User](uri)(UserBinding.userBinder)
  } { user =>
    Ok(Json.toJson(user)).as(mimeType)
  }

  def allUsers(secureContext: SecureContext = contextFrom(GetAll)) = many(secureContext) { request =>
    repository.get[User](UserBinding.userBinder, UserBinding.classUri)
  } { users =>
    Ok.chunked(chunkSimple(users)).as(mimeType)
  }

  def userAtomic(id: String, secureContext: SecureContext = contextFrom(Get)) = one(secureContext) { request =>
    val uri = s"$namespace${request.uri}".replace("/atomic", "")
    repository.get[User](uri)(UserBinding.userBinder) flatPeek atomize
  } { json =>
    Ok(json).as(mimeType)
  }

  def allUserAtomic(secureContext: SecureContext = contextFrom(GetAll)) = many(secureContext) { request =>
    repository.get[User](UserBinding.userBinder, UserBinding.classUri)
  } { users =>
    Ok.chunked(chunkAtoms(users)).as(mimeType)
  }

  def buddy(systemId: String, secureContext: SecureContext = contextFrom(Get)) = many(secureContext) { request =>
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.NaturalTrasformations._
    import bindings.StudentBinding.studentBinder

    val lwm = LWMPrefix[repository.Rdf]
    val currentUser = ((request.session(_)) andThen UUID.fromString andThen (User.generateUri(_)(namespace)))(SessionController.userId)

    val query = select ("student") where {
      **(v("student"), p(lwm.systemId), o(systemId))
    }

    repository.prepareQuery(query).
      select(_.get("student")).
      changeTo(_.headOption).
      transform(_.fold(Set.empty[String])(value => Set(value.stringValue(), currentUser))).
      requestAll(repository.getMany[Student](_)).
      run
  } { students => students.find(_.systemId == systemId).fold(
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
      ) { student =>
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

  private def gets(secureContext: SecureContext)(user: Request[AnyContent] => Try[Result]) = secureContext action { request =>
    user(request) match {
      case Success(result) =>
        result
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  private def one[A](secureContext: SecureContext)(user: Request[AnyContent] => Try[Option[A]])(toResult: A => Result) = gets(secureContext) { request =>
    user(request) map {
      case Some(a) => toResult(a)
      case None =>
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
    }
  }

  private def many[A <: User](secureContext: SecureContext)(user: Request[AnyContent] => Try[Set[A]])(toResult: Set[A] => Result) = gets(secureContext) { request =>
    user(request) map { set =>
      if (request.queryString.isEmpty)
        toResult(set)
      else
        withFilter(request.queryString)(set) match {
          case Success(filtered) =>
            toResult(filtered)
          case Failure(e) =>
            ServiceUnavailable(Json.obj(
              "status" -> "KO",
              "message" -> e.getMessage
            ))
        }
    }
  }

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
