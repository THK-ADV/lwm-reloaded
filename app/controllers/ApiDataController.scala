package controllers

import java.util.UUID

import models.security._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import store.SesameRepository
import store.bind.Bindings

import scala.util.{Failure, Success}

class ApiDataController(private val repository: SesameRepository) extends Controller {
  import repository.ops

  implicit val ns = repository.namespace
  private val bindings = Bindings(ns)

  /**
    * Authority(user: UUID, refRoles: Set[UUID], invalidated: Option[DateTime] = None, id: UUID = Authority.randomUUID)
    * RefRole(course: Option[UUID] = None, role: UUID, invalidated: Option[DateTime] = None, id: UUID = RefRole.randomUUID)
    *
    * Authority2(user: UUID, role: UUID, course: Option[UUID] = None, invalidated: Option[DateTime] = None, id: UUID = Authority.randomUUID)
    */
  // TODO
  def refRolesToAuths = Action { implicit request =>
    import bindings.{AuthorityAtomDescriptor, RefRoleAtomDescriptor, Authority2Descriptor, AuthorityDescriptor, RefRoleDescriptor}

    val result = for {
      authAtoms <- repository.getAll[AuthorityAtom]
      rrs <- repository.getAll[RefRoleAtom]
    } yield {
      val auths2 = authAtoms.toVector.flatMap { auth =>
        auth.refRoles.toVector.map {rr =>
          Authority2(auth.user.id, rr.role.id, rr.course.map(_.id), rr.invalidated)
        }
      }

      val all = authAtoms.map(a => Authority.generateUri(a.id)).map(id => repository.delete[Authority](id)) ++
        rrs.map(rr => RefRole.generateUri(rr.id)).map(id => repository.delete[RefRole](id))

      repository.addMany[Authority2](auths2).map(_ => all.map(_.isSuccess).reduce(_ && _))
    }

    result.flatten match {
      case Success(s) => Ok(Json.toJson(s))
      case Failure(e) => InternalServerError(Json.toJson(e.getMessage))
    }
  }

  def auths2 = Action { request =>
    import bindings.AuthorityAtom2Descriptor
    import models.security.Authority2.writesAtom

    repository.getAll[AuthorityAtom2] match {
      case Success(s) => Ok(Json.toJson(s))
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }
}