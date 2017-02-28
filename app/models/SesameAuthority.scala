package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._

/**
  * Structure linking a user to his/her respective authority in the system.
  * `Authority` is created in order to separate concerns between user data and
  * his/her permissions in the underlying system.
  * It abstracts over the set of all partial permissions a user has in the system.
  *
  * @param user   The referenced user
  * @param course Referenced course/module
  * @param role   Reference to `Role` Instance of that course/module
  * @param id     Unique id of the `Authority`
  */

case class SesameAuthority(user: UUID, role: UUID, course: Option[UUID] = None, invalidated: Option[DateTime] = None, id: UUID = SesameAuthority.randomUUID) extends UniqueEntity

case class SesameAuthorityProtocol(user: UUID, role: UUID, course: Option[UUID] = None)

case class SesameAuthorityAtom(user: User, role: SesameRole, course: Option[CourseAtom], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class PostgresAuthority(user: UUID, roleLabel: String, course: Option[UUID] = None, id: UUID = PostgresAuthority.randomUUID) extends UniqueEntity

case class PostgresAuthorityProtocol(user: UUID, roleLabel: String, course: Option[UUID] = None)

case class PostgresAuthorityAtom(user: User, role: PostgresRole, course: Option[CourseAtom], id: UUID) extends UniqueEntity

object SesameAuthority extends UriGenerator[SesameAuthority] with JsonSerialisation[SesameAuthorityProtocol, SesameAuthority, SesameAuthorityAtom] {

  lazy val empty = SesameAuthority(UUID.randomUUID, UUID.randomUUID)

  override def base: String = "authorities"

  override implicit def reads: Reads[SesameAuthorityProtocol] = Json.reads[SesameAuthorityProtocol]

  override implicit def writes: Writes[SesameAuthority] = Json.writes[SesameAuthority]

  override implicit def writesAtom: Writes[SesameAuthorityAtom] = SesameAuthorityAtom.writesAtom
}

object SesameAuthorityAtom {

  implicit def writesAtom: Writes[SesameAuthorityAtom] = (
    (JsPath \ "user").write[User] and
      (JsPath \ "role").write[SesameRole] and
      (JsPath \ "course").writeNullable[CourseAtom] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameAuthorityAtom.unapply))
}

object PostgresAuthority extends JsonSerialisation[PostgresAuthorityProtocol, PostgresAuthority, PostgresAuthorityAtom] {

  def randomUUID: UUID = UUID.randomUUID

  override implicit def reads = Json.reads[PostgresAuthorityProtocol]

  override implicit def writes = Json.writes[PostgresAuthority]

  override implicit def writesAtom = PostgresAuthorityAtom.writesAtom
}

object PostgresAuthorityAtom {

  implicit def writesAtom: Writes[PostgresAuthorityAtom] = (
    (JsPath \ "user").write[User] and
      (JsPath \ "role").write[PostgresRole](PostgresRole.writes) and
      (JsPath \ "course").writeNullable[CourseAtom] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresAuthorityAtom.unapply))
}