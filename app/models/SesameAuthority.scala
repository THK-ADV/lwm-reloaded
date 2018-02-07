package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime.DateTimeConverter

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

case class SesameAuthorityAtom(user: User, role: SesameRole, course: Option[SesameCourseAtom], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

// Postgres

sealed trait Authority extends UniqueEntity

case class PostgresAuthority(user: UUID, role: UUID, course: Option[UUID] = None, id: UUID = UUID.randomUUID) extends Authority

case class AuthorityDb(user: UUID, role: UUID, course: Option[UUID] = None, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresAuthority(user, role, course, id)
}

case class PostgresAuthorityProtocol(user: UUID, role: UUID, course: Option[UUID] = None)

case class PostgresAuthorityAtom(user: User, role: PostgresRole, course: Option[PostgresCourseAtom], id: UUID) extends Authority

object SesameAuthority extends UriGenerator[SesameAuthority] {
  override def base: String = "authorities"
}

object PostgresAuthority {
  implicit val writes: Writes[PostgresAuthority] = Json.writes[PostgresAuthority]
}

object PostgresAuthorityProtocol {
  implicit val reads: Reads[PostgresAuthorityProtocol] = Json.reads[PostgresAuthorityProtocol]
}

object PostgresAuthorityAtom {

  implicit val writes: Writes[PostgresAuthorityAtom] = (
    (JsPath \ "user").write[User] and
      (JsPath \ "role").write[PostgresRole](PostgresRole.writes) and
      (JsPath \ "course").writeNullable[PostgresCourseAtom](PostgresCourseAtom.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresAuthorityAtom.unapply))
}

object Authority {

  implicit val writes: Writes[Authority] = new Writes[Authority] {
    override def writes(a: Authority): JsValue = a match {
      case normal: PostgresAuthority => Json.toJson(normal)(PostgresAuthority.writes)
      case atom: PostgresAuthorityAtom => Json.toJson(atom)(PostgresAuthorityAtom.writes)
    }
  }
}