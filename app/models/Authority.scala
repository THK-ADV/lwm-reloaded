package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json._

sealed trait AuthorityLike extends UniqueEntity

case class Authority(user: UUID, role: UUID, course: Option[UUID] = None, id: UUID = UUID.randomUUID) extends AuthorityLike

case class AuthorityAtom(user: User, role: Role, course: Option[CourseAtom], id: UUID) extends AuthorityLike

case class AuthorityProtocol(user: UUID, role: UUID, course: Option[UUID] = None)

object Authority {
  implicit val writes: Writes[Authority] = Json.writes[Authority]
}

object AuthorityProtocol {
  implicit val reads: Reads[AuthorityProtocol] = Json.reads[AuthorityProtocol]
}

object AuthorityAtom {

  implicit val writes: Writes[AuthorityAtom] = (
    (JsPath \ "user").write[User] and
      (JsPath \ "role").write[Role](Role.writes) and
      (JsPath \ "course").writeNullable[CourseAtom](CourseAtom.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(AuthorityAtom.unapply))
}

object AuthorityLike {

  implicit val writes: Writes[AuthorityLike] = {
    case normal: Authority => Json.toJson(normal)(Authority.writes)
    case atom: AuthorityAtom => Json.toJson(atom)(AuthorityAtom.writes)
  }
}