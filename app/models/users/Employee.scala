package models.users

import java.util.UUID
import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Employee(systemId: String,
                    lastname: String,
                    firstname: String,
                    email: String,
                    id: Option[UUID] = Some(UUID.randomUUID())) extends User

object Employee extends UriGenerator[Employee] with JsonSerialisation[Employee] {

  override implicit def reads: Reads[Employee] = Json.reads[Employee]

  override implicit def writes: Writes[Employee] = Json.writes[Employee]

  override def base: String = "employees"
}
